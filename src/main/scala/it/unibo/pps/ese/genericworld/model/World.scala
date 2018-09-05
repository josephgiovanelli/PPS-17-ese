package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.UpdatableWorld.UpdatePolicy
import it.unibo.pps.ese.genericworld.model.UpdatableWorld.UpdatePolicy.{Deterministic, Stochastic}
import it.unibo.pps.ese.genericworld.model.support.{Done, InteractionEnvelope, InteractionEvent}

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

case class WorldInfo(width: Long, height: Long)

object EntityUpdateState extends Enumeration {
  val WAITING, UPDATING, UPDATED = Value
}

sealed trait World {
  def width: Long
  def height: Long
  def entities : Seq[Entity]
  def addEntity(entity: Entity)(implicit context: ExecutionContext): Unit
  def removeEntity(id: String): Unit
  def entitiesState : Seq[EntityState]
  def requireStateUpdate(implicit context: ExecutionContext): Future[Done]
  def requireInfoUpdate(implicit context: ExecutionContext): Future[Done]
}

sealed trait CachedWorld {
  private[this] val _queryableState = EntitiesStateCache apply

  def stateMapper: EntityState => EntityState
  def publishState(entityId: String): Unit = _queryableState publishEntityState entityId
  def hideState(entityId: String): Unit = _queryableState hideEntityState entityId
  def updateState(entityId: String, entityProperty: EntityProperty): Unit =
    _queryableState addOrUpdateEntityState (entityId, entityProperty)
  def state(filter: EntityState => Boolean, map: Boolean = true): Seq[EntityState] = {
    val state = _queryableState getFilteredState filter
    if (map) state map stateMapper else state
  }
}

sealed trait InteractiveWorld extends CachedWorld {
  def info: WorldInfo
  def addEntity(entity: Entity)(implicit context: ExecutionContext): Unit
  def removeEntity(id: String): Unit
  def interact[A <: InteractionEvent](envelope: InteractionEnvelope[A])(implicit context: ExecutionContext)
}

sealed trait UpdatableWorld {

  self: UpdatePolicy =>

  private[this] var _entityBridges : Seq[WorldBridgeComponent] = Seq empty
  private[this] var _toDeleteBridges : Set[String] = Set empty
  private[this] var _interactionSideEffects : Seq[Future[Done]] = Seq empty
  private[this] var _entitiesUpdateState : Map[String, EntityUpdateState.Value] = Map.empty

  def addBridge(bridge : WorldBridgeComponent)(implicit context: ExecutionContext): Unit = {
    _entitiesUpdateState = _entitiesUpdateState + (bridge.entitySpecifications.id -> EntityUpdateState.WAITING)
    bridge.initializeInfo().onComplete(_ => {
      _entityBridges = _entityBridges :+ bridge
    })
  }

  def removeBridge(entityId: String): Unit = _toDeleteBridges = _toDeleteBridges + entityId

  def requireStateUpdate(implicit context: ExecutionContext): Future[Done] = {
    _entitiesUpdateState = _entitiesUpdateState map { case (key, _) => key -> EntityUpdateState.WAITING }
    serializeFutures(updateOrder(_entityBridges))(e => {
      _entitiesUpdateState = _entitiesUpdateState + (e.entitySpecifications.id -> EntityUpdateState.UPDATING)
      val future = e.computeNewState flatMap (_ => Future.sequence(_interactionSideEffects.toList))
      future onComplete(_ => {
        _entityBridges filter (bridge => _toDeleteBridges contains bridge.entitySpecifications.id) foreach (x => x.dispose())
        _entityBridges = _entityBridges filterNot(bridge => _toDeleteBridges contains bridge.entitySpecifications.id)
        _interactionSideEffects = Seq.empty
        _entitiesUpdateState = _entitiesUpdateState + (e.entitySpecifications.id -> EntityUpdateState.UPDATED)
      })
      future
    }) map(_ => new Done())
  }


  def requireInfoUpdate(implicit context: ExecutionContext): Future[Done] =
    Future.traverse(_entityBridges)(e => e.requireInfo()) map (_ => new Done())

  def deliver[A <: InteractionEvent](interactionEnvelope: InteractionEnvelope[A])
                                    (implicit context: ExecutionContext): Unit = {
    _entityBridges find (x => x.entitySpecifications.id == interactionEnvelope.targetId) foreach(
      x => _interactionSideEffects = _interactionSideEffects :+ x.deliverMessage[A](interactionEnvelope))
  }

  def dynamicState: Map[String, EntityUpdateState.Value] = _entitiesUpdateState

  private def serializeFutures[A, B](l: Iterable[A])(fn: A => Future[B])
                                    (implicit context: ExecutionContext): Future[List[B]] =
    l.foldLeft(Future(List.empty[B])) {
      (previousFuture, next) =>
        for {
          previousResults <- previousFuture
          next <- fn(next)
        } yield previousResults :+ next
    }
}

object UpdatableWorld {

  sealed trait UpdatePolicy {
    def updateOrder[A](sequence: Seq[A]): Seq[A]
  }
  object UpdatePolicy {
    sealed trait Stochastic extends UpdatePolicy {
      override def updateOrder[A](sequence: Seq[A]): Seq[A] = scala.util.Random.shuffle(sequence)
    }
    sealed trait Deterministic extends UpdatePolicy {
      override def updateOrder[A](sequence: Seq[A]): Seq[A] = sequence toSeq
    }
  }
}

object World {

  import scala.reflect.runtime.universe._

  def apply[T <: UpdatePolicy](width: Long, height: Long, t: TypeTag[T]): World = t.tpe match {
    case e if e =:= typeOf[Stochastic] => new BaseInteractiveWorld(width, height) with Stochastic
    case e if e =:= typeOf[Deterministic] => new BaseInteractiveWorld(width, height) with Deterministic
  }

  private class BaseInteractiveWorld(val width: Long, val height: Long) extends World
    with InteractiveWorld with UpdatableWorld {

    self: UpdatePolicy =>

    private[this] var _entities : Seq[Entity] = Seq empty
    private[this] def entities_=(entities : Seq[Entity]) : Unit = _entities = entities

    override def info: WorldInfo = WorldInfo(width, height)

    override def entities: Seq[Entity] = _entities

    override def addEntity(entity: Entity)(implicit context: ExecutionContext): Unit = {
      entity match {
        case _: Entity with NervousSystemExtension =>
          val bridge = new WorldBridgeComponent(entity specifications, this)
          entity addComponent bridge
          addBridge(bridge)
        case _ => Unit
      }
      entities_=(entities :+ entity)
    }

    override def removeEntity(id: String): Unit = {
      removeBridge(id)
      entities find (x => x.id == id) foreach (x => x.dispose())
      entities_=(entities filterNot(e => e.id == id))
      hideState(id)
    }

    override def entitiesState: Seq[EntityState] = state (_ => true, map = false)

    override def interact[A <: InteractionEvent](envelope: InteractionEnvelope[A])
                                                (implicit context: ExecutionContext): Unit = deliver[A](envelope)

    def stateMapper: EntityState => EntityState = source => {
      val state = source.state.copy()
      state.status = dynamicState(source.entityId)
      EntityState(source.entityId, state)
    }
  }
}
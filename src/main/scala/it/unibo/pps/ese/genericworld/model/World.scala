package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support.{Done, InteractionEnvelope, InteractionEvent}

import scala.concurrent.{ExecutionContext, Future}

case class WorldInfo(width: Long, height: Long)

sealed trait World {
  def width: Long
  def height: Long
  def addEntity(entity: Entity): Unit
  def removeEntity(id: String): Unit
  def entitiesState : Seq[EntityState]
  def requireStateUpdate(implicit context: ExecutionContext): Future[Done]
  def requireInfoUpdate(implicit context: ExecutionContext): Future[Done]
}

sealed trait CachedWorld {
  private[this] val _queryableState = EntitiesStateCache apply
  def queryableState : EntitiesStateCache = _queryableState
}

sealed trait InteractiveWorld extends CachedWorld {
  def info: WorldInfo
  def entities : Seq[Entity]
  def addEntity(entity: Entity): Unit
  def removeEntity(id: String): Unit
  def interact[A <: InteractionEvent](envelope: InteractionEnvelope[A])
}

sealed trait UpdatableWorld {

  private[this] var _entityBridges : Seq[WorldBridgeComponent] = Seq empty
  private[this] var _toDeleteBridges : Set[String] = Set empty

  def addBridge(bridge : WorldBridgeComponent): Unit = _entityBridges = _entityBridges :+ bridge

  def removeBridge(entityId: String): Unit = _toDeleteBridges = _toDeleteBridges + entityId

  def requireStateUpdate(implicit context: ExecutionContext): Future[Done] =
    serializeFutures(scala.util.Random.shuffle(_entityBridges))(e => {
      val future = e.computeNewState
      future onComplete(_ => {
        _entityBridges filter (bridge => _toDeleteBridges contains bridge.entitySpecifications.id) foreach (x => x.dispose())
        _entityBridges = _entityBridges filterNot(bridge => _toDeleteBridges contains bridge.entitySpecifications.id)
      })
      future
    }) map(_ => new Done())

  def requireInfoUpdate(implicit context: ExecutionContext): Future[Done] =
    Future.traverse(_entityBridges)(e => e.requireInfo) map (_ => new Done())

  def deliver[A <: InteractionEvent](interactionEnvelope: InteractionEnvelope[A]): Unit = {
    _entityBridges find (x => x.entitySpecifications.id == interactionEnvelope.targetId) foreach(
      x => x.deliverMessage[A](interactionEnvelope))
  }

  private def serializeFutures[A, B](l: Iterable[A])(fn: A ⇒ Future[B])
                                    (implicit context: ExecutionContext): Future[List[B]] =
    l.foldLeft(Future(List.empty[B])) {
      (previousFuture, next) =>
        for {
          previousResults <- previousFuture
          next <- fn(next)
        } yield previousResults :+ next
    }
}

object World {

  def apply(width: Long, height: Long): World = BaseInteractiveWorld(width, height)

  private case class BaseInteractiveWorld(width: Long, height: Long) extends World with InteractiveWorld with UpdatableWorld {

    private[this] var _entities : Seq[Entity] = Seq empty
    private[this] def entities_=(entities : Seq[Entity]) : Unit = _entities = entities

    override def info: WorldInfo = WorldInfo(width, height)

    override def entities: Seq[Entity] = _entities

    override def addEntity(entity: Entity): Unit = {
      entity match {
        case _: Entity with NervousSystemExtension =>
          val bridge = new WorldBridgeComponent(entity specifications, this)
          addBridge(bridge)
          entity addComponent bridge
        case _ => Unit
      }
      entities_=(entities :+ entity)
    }

    override def removeEntity(id: String): Unit = {
      removeBridge(id)
      entities find (x => x.id == id) foreach (x => x.dispose())
      entities_=(entities filterNot(e => e.id == id))
      queryableState deleteEntityState id
    }

    override def entitiesState: Seq[EntityState] = queryableState getFilteredState(_ => true)

    override def interact[A <: InteractionEvent](envelope: InteractionEnvelope[A]): Unit = {
      deliver[A](envelope)
    }
  }
}
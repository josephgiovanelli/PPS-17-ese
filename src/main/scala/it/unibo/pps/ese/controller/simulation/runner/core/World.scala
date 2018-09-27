package it.unibo.pps.ese.controller.simulation.runner.core

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.{Done, InteractionEnvelope, InteractionEvent}
import it.unibo.pps.ese.controller.simulation.runner.core.UpdatableWorld.UpdatePolicy
import it.unibo.pps.ese.controller.simulation.runner.core.UpdatableWorld.UpdatePolicy.{Deterministic, Stochastic}
import it.unibo.pps.ese.controller.simulation.runner.core.data.{EntitiesStateCache, EntityProperty, EntityState, ReadOnlyEntityState}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * This class contains the world's info
  * @param width World's width
  * @param height World's height
  */
case class WorldInfo(width: Long, height: Long)

/**
  * Enumeration used internally in order to track world's entities update status
  */
object EntityUpdateState extends Enumeration {
  val INITIALIZING, WAITING, UPDATING, UPDATED = Value
}

/**
  * Main World's trait, containing the publically available APIs
  */
sealed trait World {
  /**
    * Obtain info about the simulation world
    * @return The world's info
    */
  def info(): WorldInfo

  /**
    * Get world's entities
    * @return A sequence of entities
    */
  def entities : Seq[Entity]

  /**
    * Add an entity to the simulation world
    * @param entity The entity to add
    * @param context An execution context, necessary for async tasks
    */
  def addEntity(entity: Entity)(implicit context: ExecutionContext): Unit

  /**
    * Remove an entity from the simulation world
    * @param id The target entity identifier
    */
  def removeEntity(id: String): Unit

  /**
    * Get a read only copy of the repository where the entities internal info are stored
    * @return The read only repository
    */
  def entitiesState : ReadOnlyEntityState

  /**
    * Requires an update of the entities internal status
    * @param context An execution context, necessary for async tasks
    * @return A future that will be completed at the update process end
    */
  def requireStateUpdate(implicit context: ExecutionContext): Future[Done]

  /**
    * Requires the update of the entities public status
    * @param context An execution context, necessary for async tasks
    * @return A future that will be completed at the update process end
    */
  def requireInfoUpdate(implicit context: ExecutionContext): Future[Done]
}

/**
  * Trait containing APIs related to the entities public status' repository
  */
sealed trait CachedWorld {
  private[this] val _queryableState: EntitiesStateCache = EntitiesStateCache apply

  /**
    * Get the custom mapper, used when cache's information are required
    * @return The custom mapper
    */
  def stateMapper: EntityState => EntityState

  /**
    * Get a read only copy of of the entities' cache
    * @return
    */
  def readOnlyCache: ReadOnlyEntityState = _queryableState

  /**
    * Make publically available the target entity's cached representation
    * @param entityId The entity identifier
    */
  def publishState(entityId: String): Unit = _queryableState publishEntityState entityId

  /**
    * Hide the target entity's cached representation
    * @param entityId The entity identifier
    */
  def hideState(entityId: String): Unit = _queryableState hideEntityState entityId

  /**
    * Update the entity's cached representation
    * @param entityId The entity identifier
    * @param entityProperty The property to be updated
    */
  def updateState(entityId: String, entityProperty: EntityProperty): Unit =
    _queryableState addOrUpdateEntityState (entityId, entityProperty)

  /**
    * Obtain cached entities' representation
    * @param filter A filter to be applied to the requested data
    * @param map If true, the custom mapper transformation is applied on the data
    * @return The entities' filtered representation
    */
  def state(filter: EntityState => Boolean, map: Boolean = true): Seq[EntityState] = {
    val state = _queryableState getFilteredState filter
    if (map) state map stateMapper else state
  }
}

/**
  * Trait containing APIs related to world's interaction
  */
sealed trait InteractiveWorld extends CachedWorld {

  /**
    * Obtain info about the simulation world
    * @return The world's info
    */
  def info: WorldInfo

  /**
    * Add an entity to the simulation world
    * @param entity The entity to add
    * @param context An execution context, necessary for async tasks
    */
  def addEntity(entity: Entity)(implicit context: ExecutionContext): Unit

  /**
    * Remove an entity from the simulation world
    * @param id The target entity identifier
    */
  def removeEntity(id: String): Unit

  /**
    * Send a message to an entity contained in the simulation world
    * @param envelope The envelope containing sender, target and message
    * @param context An execution context, necessary for async tasks
    * @tparam A The message type
    */
  def interact[A <: InteractionEvent](envelope: InteractionEnvelope[A])(implicit context: ExecutionContext)
}

/**
  * Trait containing APIs related to world's update process and entities communication
  */
sealed trait UpdatableWorld {

  /**
    * Requires an UpdatePolicy mix-in to be instantiated
    */
  self: UpdatePolicy =>

  private[this] var _entityBridges : Seq[WorldBridgeComponent] = Seq empty
  private[this] var _toDeleteBridges : Set[String] = Set empty
  private[this] var _toAddBridges: Set[WorldBridgeComponent] = Set empty
  private[this] var _interactionSideEffects : Seq[Future[Done]] = Seq empty
  private[this] var _entitiesUpdateState : Map[String, EntityUpdateState.Value] = Map.empty

  /**
    * Require the insertion of an entity's world bridge between the tracked ones. After the insertion the entity
    * will be included in the update process
    * @param bridge The bridge to add
    * @param context An execution context, necessary for async tasks
    */
  def addBridge(bridge : WorldBridgeComponent)(implicit context: ExecutionContext): Unit = this synchronized {
    _entitiesUpdateState = _entitiesUpdateState + (bridge.entitySpecifications.id -> EntityUpdateState.INITIALIZING)
    _toAddBridges = _toAddBridges + bridge
  }

  /**
    * Require te deletion of an entity's world bridge. After the deletion the entity will be no more considered in
    * the update process and the related entity won't be reachable from other entities
    * @param entityId The target entity's identifier
    */
  def removeBridge(entityId: String): Unit = _toDeleteBridges = _toDeleteBridges + entityId

  /**
    * Requires an update of the entities internal status. During the update process tasks related to bridges
    * addition and removal are performed
    * @param context An execution context, necessary for async tasks
    * @return A future that will be completed at the update process end
    */
  def requireStateUpdate(implicit context: ExecutionContext): Future[Done] = {

    def updateState(entityId: String = "ALL", newState: EntityUpdateState.Value): Unit = this synchronized {
      entityId match {
        case "ALL" => _entitiesUpdateState = _entitiesUpdateState map { case (key, _) => key -> newState }
        case _ => _entitiesUpdateState = _entitiesUpdateState + (entityId -> newState)
      }
    }

    def updateRoutine(): WorldBridgeComponent => Future[Done] = bridge => {
      updateState(bridge.entitySpecifications.id, EntityUpdateState.UPDATING)
      bridge.computeNewState flatMap (_ => Future.sequence(_interactionSideEffects.toList)) andThen {
        case Success(_) =>
          removeDeletedEntities()
          _interactionSideEffects = Seq.empty
          updateState(bridge.entitySpecifications.id, EntityUpdateState.UPDATED)
        case Failure(exception) => throw exception
      } map(_ => new Done())
    }

    import it.unibo.pps.ese.controller.simulation.runner.core.support.FutureHelper._

    updateState(newState = EntityUpdateState.WAITING)
    initializeNewEntities() flatMap (_ => Future.serializeFutures(updateOrder(_entityBridges))(updateRoutine())) map (_ => new Done())
  }

  /**
    * Requires the update of the entities public status
    * @param context An execution context, necessary for async tasks
    * @return A future that will be completed at the update process end
    */
  def requireInfoUpdate(implicit context: ExecutionContext): Future[Done] =
    initializeNewEntities() flatMap (_ => Future.traverse(_entityBridges)(e => e.requireInfo())) map (_ => new Done())

  /**
    * Send a message to an entity contained in the simulation world using the entity's bridge
    * @param interactionEnvelope The envelope containing sender, target and message
    * @param context An execution context, necessary for async tasks
    * @tparam A The message type
    */
  def deliver[A <: InteractionEvent](interactionEnvelope: InteractionEnvelope[A])
                                    (implicit context: ExecutionContext): Unit = {
    _entityBridges find (x => x.entitySpecifications.id == interactionEnvelope.targetId) foreach(
      x => _interactionSideEffects = _interactionSideEffects :+ x.deliverMessage[A](interactionEnvelope))
  }

  /**
    * Get world's entities update status
    * @return Entities update status
    */
  def dynamicState: Map[String, EntityUpdateState.Value] = _entitiesUpdateState

  /**
    * Bridges insertion routine
    * @param context An execution context, necessary for async tasks
    * @return A future that will be completed at the process end
    */
  private def initializeNewEntities()(implicit context: ExecutionContext): Future[Done] = {
    val bridges: Set[WorldBridgeComponent] = this synchronized {
      val temp = _toAddBridges
      _toAddBridges = Set.empty
      temp
    }
    Future.sequence(bridges map(_ initializeInfo())) andThen {
      case Success(_) =>
        _entityBridges = _entityBridges ++: bridges.toSeq
      case Failure(e) => throw e
    } map(_ => new Done)
  }

  /**
    * Bridges deletion routine
    */
  private def removeDeletedEntities(): Unit = {
    _entityBridges filter (bridge => _toDeleteBridges contains bridge.entitySpecifications.id) foreach (x => x.dispose())
    _entityBridges = _entityBridges filterNot(bridge => _toDeleteBridges contains bridge.entitySpecifications.id)
  }
}

object UpdatableWorld {

  /**
    * This trait defines the policy to use during the update process
    */
  sealed trait UpdatePolicy {
    def updateOrder[A <: WorldBridgeComponent](sequence: Seq[A]): Seq[A]
  }
  object UpdatePolicy {

    /**
      * Entities are updated in a random order
      */
    sealed trait Stochastic extends UpdatePolicy {
      override def updateOrder[A <: WorldBridgeComponent](sequence: Seq[A]): Seq[A] = scala.util.Random.shuffle(sequence)
    }

    /**
      * Entities are updated in a deterministic order
      */
    sealed trait Deterministic extends UpdatePolicy {
      override def updateOrder[A <: WorldBridgeComponent](sequence: Seq[A]): Seq[A] = sequence.sortWith((a1, a2) => a1.entitySpecifications.id < a2.entitySpecifications.id)
    }
  }
}

object World {

  import scala.reflect.runtime.universe._

  /**
    * @param width World's width
    * @param height World's height
    * @param t The TypeTag associated with the requested update policy
    * @tparam T The type of the UpdatePolicy to be used during the world's update process
    * @return A World instance
    */
  def apply[T <: UpdatePolicy](width: Long, height: Long)(implicit t: TypeTag[T]): World = t.tpe match {
    case e if e =:= typeOf[Stochastic] => new BaseInteractiveWorld(width, height) with Stochastic
    case e if e =:= typeOf[Deterministic] => new BaseInteractiveWorld(width, height) with Deterministic
  }

  private class BaseInteractiveWorld(width: Long, height: Long) extends World
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
      this synchronized { entities_=(entities :+ entity) }
    }

    override def removeEntity(id: String): Unit = {
      removeBridge(id)
      entities find (x => x.id == id) foreach (x => x.dispose())
      this synchronized { entities_=(entities filterNot(e => e.id == id)) }
      hideState(id)
    }

    override def entitiesState: ReadOnlyEntityState = readOnlyCache

    override def interact[A <: InteractionEvent](envelope: InteractionEnvelope[A])
                                                (implicit context: ExecutionContext): Unit = deliver[A](envelope)

    def stateMapper: EntityState => EntityState = source => {
      val state = source.state.copy()
      state.status = dynamicState(source.entityId)
      EntityState(source.entityId, state)
    }
  }
}
package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support.{BaseEvent, Done}

import scala.concurrent.{ExecutionContext, Future, Promise}

case class RequireEntitiesState(id: String, filter: EntityState => Boolean = _ => true) extends BaseEvent
case class EntitiesStateResponse(id: String, state: Seq[EntityState]) extends BaseEvent
case class CreateEntity(entity: Entity) extends BaseEvent
case class KillEntity(entityId: String) extends BaseEvent
case class NewState(properties: Seq[EntityProperty]) extends BaseEvent
case class ComputeNextState() extends BaseEvent
case class ComputeNextStateResponse() extends BaseEvent
case class GetInfo() extends BaseEvent
case class GetInfoResponse() extends BaseEvent

sealed trait WorldBridge {
  def computeNewState(implicit context: ExecutionContext): Future[Done]
  def requireInfo(implicit context: ExecutionContext): Future[Done]
}

class WorldBridgeComponent(override val entitySpecifications: EntitySpecifications, world: InteractiveWorld) extends WriterComponent(entitySpecifications) with WorldBridge {

  private var newStatePromise : Promise[Done] = Promise()
  private var requireInfoPromise : Promise[Done] = Promise()
  private var newStateAccumulator : Long = 0
  private var requireInfoAccumulator : Long = 0

  newStatePromise success new Done()
  requireInfoPromise success new Done()

  override def initialize(): Unit = subscribe {
    case RequireEntitiesState(id, filter) => publish(EntitiesStateResponse(id, (world queryableState) getFilteredState filter))
    case CreateEntity(entity) => world addEntity entity
    case KillEntity(entityId) => world removeEntity entityId
    case NewState(properties) => properties foreach (e => (world queryableState) addOrUpdateEntityState (entitySpecifications id, e))
    case ComputeNextStateResponse() =>
      newStateAccumulator += 1
      if (newStateAccumulator == entitySpecifications.componentsCount - 1) newStatePromise success new Done()
    case GetInfoResponse() =>
      requireInfoAccumulator += 1
      if (requireInfoAccumulator == entitySpecifications.componentsCount - 1) requireInfoPromise success new Done()
    case _ => Unit
  }

  override def computeNewState(implicit context: ExecutionContext): Future[Done] = {
    if (newStatePromise isCompleted) {
      newStatePromise = Promise()
      newStateAccumulator = 0
      publish(ComputeNextState())
      newStatePromise future
    } else {
      failurePromise()
    }
  }

  override def requireInfo(implicit context: ExecutionContext): Future[Done] = {
    if (requireInfoPromise isCompleted) {
      requireInfoPromise = Promise()
      requireInfoAccumulator = 0
      publish(GetInfo())
      requireInfoPromise future
    } else {
      failurePromise()
    }
  }

  private def failurePromise() : Future[Done] = {
    val result = Promise[Done]()
    result failure new RuntimeException("Pending request")
    result future
  }
}

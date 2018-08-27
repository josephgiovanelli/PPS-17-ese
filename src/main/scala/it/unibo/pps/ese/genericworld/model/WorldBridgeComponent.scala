package it.unibo.pps.ese.genericworld.model

import java.util.concurrent.atomic.AtomicLong
import it.unibo.pps.ese.genericworld.model.support._

import scala.concurrent.{ExecutionContext, Future, Promise}

case class UpdateEntityState(properties: Seq[EntityProperty]) extends BaseEvent

case class WorldInfoRequest() extends RequestEvent
case class WorldInfoResponse(override val id: String, width: Long, height: Long) extends ResponseEvent(id)

case class EntitiesStateRequest(filter: EntityState => Boolean = _ => true) extends RequestEvent
case class EntitiesStateResponse(override val id: String, state: Seq[EntityState]) extends ResponseEvent(id)

case class ComputeNextState() extends BaseEvent
case class ComputeNextStateAck() extends BaseEvent

case class Kill(entityId: String) extends BaseEvent
case class Create(partnerId: String) extends BaseEvent

case class GetInfo() extends BaseEvent
case class GetInfoAck() extends BaseEvent

sealed trait WorldBridge {
  def computeNewState(implicit context: ExecutionContext): Future[Done]
  def requireInfo(implicit context: ExecutionContext): Future[Done]
  def deliverMessage[A <: InteractionEvent](envelope: InteractionEnvelope[A])
  def dispose(): Unit
}

class WorldBridgeComponent(override val entitySpecifications: EntitySpecifications,
                           world: InteractiveWorld) extends WriterComponent(entitySpecifications) with WorldBridge {

  private var disposed = false
  private var newStatePromise : Promise[Done] = Promise()
  private var requireInfoPromise : Promise[Done] = Promise()
  private val newStateAccumulator : AtomicLong = new AtomicLong(0)
  private val requireInfoAccumulator : AtomicLong = new AtomicLong(0)

  newStatePromise success new Done()
  requireInfoPromise success new Done()

  override def initialize(): Unit = subscribe {
    case r: WorldInfoRequest =>
      publish(WorldInfoResponse(r id, (world info) width, (world info) height))
    case r: EntitiesStateRequest =>
      publish(EntitiesStateResponse(r id,
        (world queryableState) getFilteredState r.filter filterNot (x => x.entityId == entitySpecifications.id)))
    case r: InteractionEvent if r.id != entitySpecifications.id =>
      if (!disposed) world interact InteractionEnvelope(entitySpecifications id, r id, r)
    case UpdateEntityState(properties) => properties foreach (e =>
      if (!disposed) (world queryableState) addOrUpdateEntityState (entitySpecifications id, e))
    case Kill(entityId)  =>
      if (!disposed) world removeEntity entityId
    case Create(partnerId)  =>
      //create a new entity based on both entities features
      //and then call
      //world addEntity newEntity
    case ComputeNextStateAck() =>
      if (newStateAccumulator.incrementAndGet == entitySpecifications.componentsCount - 1)
        newStatePromise success new Done()
    case GetInfoAck() =>
      if (requireInfoAccumulator.incrementAndGet == entitySpecifications.componentsCount - 1)
        requireInfoPromise success new Done()
    case _ => Unit
  }

  override def computeNewState(implicit context: ExecutionContext): Future[Done] = {
    if (disposed) Future {new Done}
    if (newStatePromise isCompleted) {
      newStatePromise = Promise()
      newStateAccumulator.set(0)
      publish(ComputeNextState())
      newStatePromise future
    } else {
      failurePromise()
    }
  }

  override def requireInfo(implicit context: ExecutionContext): Future[Done] = {
    if (disposed) Future {new Done}
    if (requireInfoPromise isCompleted) {
      requireInfoPromise = Promise()
      requireInfoAccumulator.set(0)
      publish(GetInfo())
      requireInfoPromise future
    } else {
      failurePromise()
    }
  }

  override def deliverMessage[A <: InteractionEvent](envelope: InteractionEnvelope[A]): Unit =
    publish(envelope.message)

  override def dispose(): Unit = disposed = true

  private def failurePromise() : Future[Done] = {
    val result = Promise[Done]()
    result failure new RuntimeException("Pending request")
    result future
  }
}

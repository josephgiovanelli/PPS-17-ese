package it.unibo.pps.ese.genericworld.model

import java.util.concurrent.atomic.AtomicLong

import it.unibo.pps.ese.genericworld.model.support._
import it.unibo.pps.ese.genetics.entities.AnimalInfo

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

case class UpdateEntityState(properties: Seq[EntityProperty]) extends BaseEvent

case class WorldInfoRequest() extends RequestEvent
case class WorldInfoResponse(override val id: String, width: Long, height: Long) extends ResponseEvent

case class EntitiesStateRequest(filter: EntityState => Boolean = _ => true) extends RequestEvent
case class EntitiesStateResponse(override val id: String, state: Seq[EntityState]) extends ResponseEvent

case class EntityExecutionRequest(entityId: String) extends RequestEvent
case class EntityExecutionResponse(override val id: String, status: Boolean) extends ResponseEvent

case class ComputeNextState() extends BaseEvent with HighPriorityEvent
case class ComputeNextStateAck() extends BaseEvent with HighPriorityEvent

case class Kill(entityId: String) extends BaseEvent
case class Create(sons: Iterable[AnimalInfo]) extends BaseEvent

case class GetInfo() extends BaseEvent with HighPriorityEvent
case class GetInfoAck() extends BaseEvent with HighPriorityEvent

sealed trait WorldBridge {
  def initializeInfo(): Future[Done]
  def computeNewState(): Future[Done]
  def requireInfo(): Future[Done]
  def deliverMessage[A <: InteractionEvent](envelope: InteractionEnvelope[A]): Future[Done]
  def dispose(): Unit
}

class WorldBridgeComponent(override val entitySpecifications: EntitySpecifications,
                           world: InteractiveWorld)
                          (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications) with WorldBridge {

  private var disposed = false
  private var jobCompleted = true
  private var runningJobPromise : Promise[Done] = Promise()
  private val runningJobAccumulator : AtomicLong = new AtomicLong(0)

  runningJobPromise success new Done()

  override def initialize(): Unit = subscribe {
    case r: WorldInfoRequest =>
      publish(WorldInfoResponse(r id, (world info) width, (world info) height))
    case r: EntitiesStateRequest =>
      publish(EntitiesStateResponse(r id,
        world.state(r.filter)filterNot(x => x.entityId == entitySpecifications.id)))
    case r: InteractionEvent if r.receiverId != entitySpecifications.id =>
      if (!disposed) world interact InteractionEnvelope(entitySpecifications id, r receiverId, r)
    case UpdateEntityState(properties) => properties foreach (e =>
      if (!disposed) world updateState (entitySpecifications id, e))
    case Kill(entityId)  =>
      //Completed before next entity computation?
      if (!disposed) world removeEntity entityId
    case Create(sons)  =>
      println(sons.size + " entities Creation")
      //Completed before next entity computation?
      if (!disposed) //Necessary?
      requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest).onComplete({
        case Success(info) =>
          sons.map(i => EntityBuilderHelpers.initializeEntity(i, info.position, world.info.height, world.info.width))
              .foreach(entity =>world addEntity entity)
        case Failure(exception) =>
          exception
      })
    case ComputeNextStateAck() =>
      runningJobAccumulator.incrementAndGet
      checkRunningJobCompletion()
    case GetInfoAck() =>
      runningJobAccumulator.incrementAndGet
      checkRunningJobCompletion()
    case _ => Unit
  }

  override def computeNewState(): Future[Done] = startNewJob(ComputeNextState())

  override def requireInfo(): Future[Done] = startNewJob(GetInfo())

  override def deliverMessage[A <: InteractionEvent](envelope: InteractionEnvelope[A]): Future[Done] = {
    val interactionPromise = Promise[Done]
    nervousSystem notifyOnTasksEnd() onComplete (_ => {
      interactionPromise success new Done()
    })
    publish(envelope.message)
    interactionPromise future
  }

  override def dispose(): Unit = disposed = true

  private def startNewJob(event: Event): Future[Done] = {
    if (disposed) Future {new Done}
    if ((runningJobPromise isCompleted) && jobCompleted) {
      jobCompleted = false
      nervousSystem notifyOnTasksEnd() onComplete (_ => {
        jobCompleted = true
        checkRunningJobCompletion()
      })
      runningJobPromise = Promise()
      runningJobAccumulator.set(0)
      publish(event)
      runningJobPromise future
    } else {
      failurePromise()
    }
  }

  private def checkRunningJobCompletion(): Unit = this synchronized {
    if (!(runningJobPromise isCompleted)
      && runningJobAccumulator.get() == entitySpecifications.componentsCount - 1
      && jobCompleted) {
      runningJobPromise success new Done()
    }
  }

  private def failurePromise() : Future[Done] = {
    val result = Promise[Done]()
    result failure new RuntimeException("Pending request")
    result future
  }

  override def initializeInfo(): Future[Done] = requireInfo().map(result => {
    world.publishState(entitySpecifications id)
    result
  })
}

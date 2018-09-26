package it.unibo.pps.ese.controller.simulation.runner.core

import java.util.concurrent.atomic.AtomicLong

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport._
import it.unibo.pps.ese.controller.simulation.runner.core.data.{EntityProperty, EntityState}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}


/**
  * This trait contains the APIs used to communicate with an entity and control it
  */
sealed trait WorldBridge {

  /**
    * Require the initialization of the entity's public state
    * @return A future that will be completed at the end of the initialization process
    */
  def initializeInfo(): Future[Done]

  /**
    * Require the update of entities' internal status
    * @return A future that will be completed at the end of the update process
    */
  def computeNewState(): Future[Done]

  /**
    * Require the update of entities' public status
    * @return A future that will be completed at the end of the update process
    */
  def requireInfo(): Future[Done]

  /**
    * Send a message to the entity from outside
    * @param envelope The message to be delivered
    * @tparam A The message type
    * @return A future that will be completed when after message reception
    */
  def deliverMessage[A <: InteractionEvent](envelope: InteractionEnvelope[A]): Future[Done]

  /**
    * Dispose entity's resources
    */
  def dispose(): Unit
}

/**
  * The WorldBridge is implemented as a normal component, so to interact with others components
  * and propagate world decisions the NervousSystem communication medium must be used.
  * These classes represent also the main functionalities that the WorldBridgeComponent offers to custom
  * ones.
  */

/**
  * Update the public entities's state cache
  * @param properties The properties to update
  */
case class UpdateEntityState(properties: Seq[EntityProperty]) extends BaseEvent

/**
  * Require info about the simulation world
  */
case class WorldInfoRequest() extends RequestEvent
case class WorldInfoResponse(override val id: String, width: Long, height: Long) extends ResponseEvent

/**
  * Require info about other simulation entities
  * @param filter The filter to be applied to entities' state cache while fetching the info
  */
case class EntitiesStateRequest(filter: EntityState => Boolean = _ => true) extends RequestEvent
case class EntitiesStateResponse(override val id: String, state: Seq[EntityState]) extends ResponseEvent

/**
  * Require the update of entities' internal status
  */
case class ComputeNextState() extends BaseEvent with HighPriorityEvent
case class ComputeNextStateAck() extends BaseEvent with HighPriorityEvent

/**
  * Require the update of entities' public status
  */
case class GetInfo() extends BaseEvent with HighPriorityEvent
case class GetInfoAck() extends BaseEvent with HighPriorityEvent

/**
  * Require the removal of an entity from the simulation world
  * @param entityId The target entity identifier
  */
case class Kill(entityId: String) extends BaseEvent

/**
  * Require the addition of an entity to the simulation world
  * @param entities The entities to be added
  */
case class Create(entities: Iterable[Entity]) extends BaseEvent

/**
  * Notify the addition of entities to the simulation world
  * @param ids The added entities
  */
case class GiveBirth(ids: Seq[String]) extends BaseEvent

class WorldBridgeComponent(override val entitySpecifications: EntitySpecifications,
                           world: InteractiveWorld)
                          (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications) with WorldBridge {

  private var disposed = false
  private var jobCompleted = true
  private var runningJobPromise : Promise[Done] = Promise()
  private val runningJobAccumulator : AtomicLong = new AtomicLong(0)

  runningJobPromise success new Done()

  override def initialize(): Unit = subscribe {
    case r: WorldInfoRequest => publish(WorldInfoResponse(r id, (world info) width, (world info) height))
    case r: EntitiesStateRequest =>
      publish(EntitiesStateResponse(r id, world.state(r.filter)filterNot(x => x.entityId == entitySpecifications.id)))
    case r: InteractionEvent if r.receiverId != entitySpecifications.id =>
      if (!disposed) world interact InteractionEnvelope(entitySpecifications id, r receiverId, r)
    case UpdateEntityState(properties) => properties foreach (e =>
      if (!disposed) world updateState (entitySpecifications id, e))
    case Kill(entityId)  =>
      if (!disposed) world removeEntity entityId
    case Create(entities)  =>
      if (!disposed) publish(GiveBirth(entities.map(entity => { world addEntity entity; entity id}).toSeq))
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

  override def initializeInfo(): Future[Done] = requireInfo() andThen {
    case Success(_) => world.publishState(entitySpecifications id)
    case Failure(e) => throw e
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
}

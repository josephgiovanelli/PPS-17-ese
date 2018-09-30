package it.unibo.pps.ese.controller.simulation.runner.core

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.controller.simulation.runner.core.support._

import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * The NervousSystem is the communication medium used by entities' components to cooperate and share info
  */
sealed trait NervousSystem {

  /**
    * Publish a message on the nervous system. This event will be delivered to all others components
    * @param event The message to deliver
    */
  def publish(event: IdentifiedEvent) : Unit

  /**
    * Register a consumer on the nervous system in order to be notified when messages of the selected type
    * are delivered
    * @param handler The consumer to register
    */
  def subscribe(handler: Consumer)

  /**
    * Send a data request. This method is equivalent to publish a message and register the response consumer
    * @param publisher The sender id
    * @param request The request message
    * @tparam A The request type
    * @tparam B The response type
    * @return A supervised future that will be completed with the requested data
    */
  def requireData[A <: RequestEvent, B <: ResponseEvent : Manifest](publisher: String, request: A): SupervisedFuture[B]

  /**
    * Register a mapping from an event to a sequence of entity properties. After the registration, when messages of the
    * mapped type will be shared across the NervousSystem, the system will automatically extract the properties from
    * it and use them to update the entity public status
    * @param mapper The mapper to register
    * @tparam A The type of the message to map
    */
  def addMapping[A <: Event](mapper: (Class[A], A => Seq[EntityProperty]))

  /**
    * Verify if the nervous system has completed all of its jobs
    * @return A future that will be completed when all the messages sent through the system are served
    */
  def notifyOnTasksEnd(): Future[Done]
}

/**
  * Traits that extends the capabilities of a NervousSystem adding the possibility to free the NervousSystem
  * resources
  */
sealed trait ManageableNervousSystem extends NervousSystem {
  /**
    * Dispose NervousSystem resources
    */
  def dispose(): Unit
}

object NervousSystem {

  /**
    * @param executionContext An execution context, necessary for async tasks
    * @return A NervousSystem instance
    */
  def apply()(implicit executionContext: ExecutionContext): ManageableNervousSystem = new SnifferNervousSystem()

  private class SnifferNervousSystem()(implicit executionContext: ExecutionContext) extends ManageableNervousSystem {

    private[this] val _eventBus = EventBus()
    private[this] var _eventsMappings : List[(Class[_], _ => Seq[EntityProperty])]= List empty
    private[this] val _mapper : Event => Unit = ev => {
      _eventsMappings filter(e => e._1.getName == ev.getClass.getName) foreach {
        case (_, map: (Event => Seq[EntityProperty])) =>
          publish(IdentifiedEvent(getClass.getSimpleName, UpdateEntityState(map(ev))))
        case _ => Unit
      }
    }

    _eventBus attach IdentifiedConsumer(getClass.getSimpleName, _mapper)

    override def publish(event: IdentifiedEvent) : Unit = _eventBus send event

    override def subscribe(handler: Consumer) : Unit = _eventBus attach handler

    override def addMapping[A <: Event](mapper: (Class[A], A => Seq[EntityProperty])): Unit =
      _eventsMappings = mapper :: _eventsMappings

    override def requireData[A <: RequestEvent, B <: ResponseEvent: Manifest](publisher: String, request: A): SupervisedFuture[B] = {
      val result = Promise[B]()
      lazy val consumer : Consumer = IdentifiedConsumer(publisher, {
        case response: B if response.id == request.id =>
          result success response
          _eventBus detach consumer
        case _ => Unit
      })
      _eventBus attach consumer
      _eventBus send IdentifiedEvent(publisher, request)

      implicit val supervisor: Supervisor = new Supervisor {
        override def computationStarted: () => Unit = () => _eventBus notifyNewTaskStart()
        override def computationEnded: () => Unit = () => _eventBus notifyNewTaskEnd()
      }
      new SupervisedFuture(result.future)
    }

    override def dispose(): Unit = {
      _eventBus detach IdentifiedConsumer(getClass.getSimpleName, _mapper)
      _eventsMappings = List empty
    }

    override def notifyOnTasksEnd(): Future[Done] = _eventBus notifyOnTasksEnd()
  }
}
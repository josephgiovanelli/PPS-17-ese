package it.unibo.pps.ese.controller.simulation.runner.core.support

import java.util.concurrent.atomic.AtomicLong

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/**
  * Public APIs of a basic event bus implementation
  */
sealed trait EventBus {

  /**
    * Send an event on the bus
    * @param event The event to be sent
    */
  def send(event: IdentifiedEvent): Unit

  /**
    * Attach a listener for the events of the bus
    * @param e The listener to be registered
    */
  def attach(e: Consumer): Unit

  /**
    * Detach a registered listener
    * @param consumer The listner to detach
    */
  def detach(consumer: Consumer): Unit

  /**
    * Notify the bus that a side affect related to a served event has commence
    */
  def notifyNewTaskStart(): Unit

  /**
    * Notify the bus that a side affect related to a served event has ended
    */
  def notifyNewTaskEnd(): Unit

  /**
    * Require the bus to be notified when all events are served and all the associated side effects completed
    * their execution
    * @return A future that will be completed on tasks end
    */
  def notifyOnTasksEnd(): Future[Done]
}

object EventBus {
  /*
    * @param executionContext An execution context, required for async tasks
    * @return An EventBus instance
    */
  def apply()(implicit executionContext: ExecutionContext): EventBus = new PriorityEventBus()

  /**
    * In this implementation of the EventBus trait all event are served asynchronously and in respect
    * of their priority
    * @param executionContext
    */
  private class PriorityEventBus()(implicit executionContext: ExecutionContext) extends EventBus {

    case class EventInfo(event: Event, f: Event => Unit) {
      def execute: Future[Any] = Future{f(event)}
    }

    private[this] var consumersRegistry = List[Consumer]()
    private[this] val activeTasks = new AtomicLong(0)
    private[this] var completionPromise: Option[Promise[Done]] = None

    private[this] var delayedHighPriorityEvents = Seq[EventInfo]()
    private[this] var delayedLowPriorityEvents = Seq[EventInfo]()
    private[this] val activeHighPriorityTasks = new AtomicLong(0)

    override def send(i: IdentifiedEvent): Unit = {

      def serveEvent(eventInfo: EventInfo): Unit =
        eventInfo.execute.onComplete{
          case Success(_) =>
            checkTasksCompletion()
            eventInfo event match {
              case _: HighPriorityEvent => activeHighPriorityTasks decrementAndGet()
              case _ => Unit
            }
            dequeueAndServe()
          case Failure(error) => throw error
        }

      def enqueueEvent(eventInfo: EventInfo): Unit = this synchronized {
        eventInfo event match {
          case _: HighPriorityEvent => delayedHighPriorityEvents = delayedHighPriorityEvents :+ eventInfo
          case _ => delayedLowPriorityEvents = delayedLowPriorityEvents :+ eventInfo
        }
      }

      def dequeueAndServe(): Unit = {
        def events(): Seq[EventInfo] = this synchronized {
          if (delayedHighPriorityEvents.nonEmpty) {
            val temp = delayedHighPriorityEvents
            activeHighPriorityTasks addAndGet temp.size
            delayedHighPriorityEvents = Seq[EventInfo]()
            temp
          } else if (delayedLowPriorityEvents.nonEmpty && activeHighPriorityTasks.get() == 0) {
            val temp = delayedLowPriorityEvents
            delayedLowPriorityEvents = Seq[EventInfo]()
            temp
          } else {
            Seq[EventInfo]()
          }
        }

        events() foreach serveEvent
      }

      consumersRegistry
        .filterNot (x => x.sourceId == i.sourceId)
        .map(x => x consumer)
        .foreach (f => {
          activeTasks.incrementAndGet()
          enqueueEvent(EventInfo(i event, f))
        })

      dequeueAndServe()
    }

    override def notifyOnTasksEnd(): Future[Done] = this synchronized {
      if (completionPromise isEmpty) completionPromise = Some(Promise[Done])
      (completionPromise get) future
    }

    override def attach(e : Consumer): Unit = this synchronized {
      consumersRegistry = e :: consumersRegistry
    }

    override def detach(consumer: Consumer): Unit = this synchronized {
      consumersRegistry = consumersRegistry filterNot (_ == consumer)
    }

    override def notifyNewTaskStart(): Unit = activeTasks.incrementAndGet()

    override def notifyNewTaskEnd(): Unit = checkTasksCompletion()

    private[this] def checkTasksCompletion(): Unit = this synchronized {
      if (activeTasks.decrementAndGet() == 0 && completionPromise.isDefined && !completionPromise.get.isCompleted) {
        (completionPromise get) success new Done
        completionPromise = None
      }
    }
  }
}
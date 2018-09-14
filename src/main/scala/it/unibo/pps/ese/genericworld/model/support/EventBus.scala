package it.unibo.pps.ese.genericworld.model.support

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}


sealed trait EventBus extends Serializable {
  def send(event: IdentifiedEvent): Unit
  def attach(e: Consumer): Unit
  def detach(consumer: Consumer): Unit
  def notifyNewTaskStart(): Unit
  def notifyNewTaskEnd(): Unit
  def notifyOnTasksEnd(): Future[Done]
}

object EventBus {
  def apply()(implicit executionContext: ExecutionContext): EventBus = new BaseEventBus()

  @SerialVersionUID(100L)
  private class BaseEventBus()(implicit executionContext: ExecutionContext) extends EventBus {

    case class EventInfo(event: Event, f: Event => Unit) {
      def execute: Future[Any] = Future{f(event)}
    }

    private[this] var consumersRegistry = List[Consumer]()
    private[this] var activeTasks = new AtomicLong(0)
    private[this] var completionPromise: Option[Promise[Done]] = None

    private[this] var delayedHighPriorityEvents = Seq[EventInfo]()
    private[this] var delayedLowPriorityEvents = Seq[EventInfo]()
    private[this] var activeHighPriorityTasks = new AtomicLong(0)

    case class EventBusMemento(consumersRegistry: List[Consumer],
                               activeTasks: AtomicLong,
                               completionPromise: Option[Promise[Done]],
                               delayedHighPriorityEvents: Seq[EventInfo],
                               delayedLowPriorityEvents: Seq[EventInfo],
                               activeHighPriorityTasks: AtomicLong)


    @throws(classOf[IOException])
    private def writeObject(out: ObjectOutputStream): Unit ={
      val em = EventBusMemento(
        consumersRegistry,
        activeTasks,
        completionPromise,
        delayedHighPriorityEvents,
        delayedLowPriorityEvents,
        activeHighPriorityTasks
      )
      out.writeObject(em)
    }

    @throws(classOf[IOException])
    private def readObject(in: ObjectInputStream): Unit = {
      val em = in.readObject.asInstanceOf[EventBusMemento]

      consumersRegistry = em.consumersRegistry
      activeTasks = em.activeTasks
      completionPromise = em.completionPromise
      delayedHighPriorityEvents = em.delayedHighPriorityEvents
      delayedLowPriorityEvents = em.delayedHighPriorityEvents
      activeHighPriorityTasks = em.activeHighPriorityTasks
    }

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
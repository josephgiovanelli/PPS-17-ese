package it.unibo.pps.ese.genericworld.model.support

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

sealed trait EventBus {
  def send(event: Event): Unit
  def attach(e: Consumer): Unit
  def detach(e: Consumer): Unit
  def notifyNewTaskStart(): Unit
  def notifyNewTaskEnd(): Unit
  def notifyOnTasksEnd(): Future[Done]
}

object EventBus {
  def apply()(implicit executionContext: ExecutionContext): EventBus = new BaseEventBus()

  private class BaseEventBus()(implicit executionContext: ExecutionContext) extends EventBus {

    private[this] var consumersRegistry = List[Consumer]()
    private[this] val activeTasks = new AtomicLong(0)
    private[this] var completionPromise: Option[Promise[Done]] = None

    override def send(event: Event): Unit = {
      consumersRegistry foreach ( f => {
        activeTasks.incrementAndGet()
        Future{f(event)}
          .onComplete{
            case Success(_) => checkTasksCompletion()
            case Failure(error) => throw error
          }
      })
    }

    override def notifyOnTasksEnd(): Future[Done] = this synchronized {
      if (completionPromise isEmpty) completionPromise = Some(Promise[Done])
      (completionPromise get) future
    }

    override def attach(e : Consumer): Unit = this synchronized {
      consumersRegistry = e :: consumersRegistry
    }

    override def detach(e: Consumer): Unit = this synchronized {
      consumersRegistry = consumersRegistry filterNot (e == _)
    }

    private def checkTasksCompletion(): Unit = this synchronized {
      if (activeTasks.decrementAndGet() == 0 && completionPromise.isDefined && !completionPromise.get.isCompleted) {
        (completionPromise get) success new Done
        completionPromise = None
      }
    }

    override def notifyNewTaskStart(): Unit = activeTasks.incrementAndGet()

    override def notifyNewTaskEnd(): Unit = activeTasks.decrementAndGet()
  }
}
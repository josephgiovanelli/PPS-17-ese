package it.unibo.pps.ese.genericworld.model.support

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

sealed trait EventBus {
  def send(event: Event): Unit
  def attach(e: Consumer): Unit
  def detach(e: Consumer): Unit
}

object EventBus {
  def apply(): EventBus = new BaseEventBus()

  private class BaseEventBus() extends EventBus {

    private[this] var consumersRegistry = List[Consumer]()

    override def send(event: Event): Unit = {
      consumersRegistry foreach { f =>
        Future{f(event)}
          .onComplete{
            case Success(_) => Unit
            case Failure(error) => throw error
          }
      }
    }

    override def attach(e : Consumer): Unit = this synchronized {
      consumersRegistry = e :: consumersRegistry
    }

    override def detach(e: Consumer): Unit = this synchronized {
      consumersRegistry = consumersRegistry filterNot (e == _)
    }
  }
}
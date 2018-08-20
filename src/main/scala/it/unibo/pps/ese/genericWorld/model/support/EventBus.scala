package it.unibo.pps.ese.genericWorld.model.support

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

sealed trait EventBus {
  def send(event: Event): Unit
  def attach(e: Consumer): Unit
}

object EventBus {
  def apply(): EventBus = BaseEventBus()

  private case class BaseEventBus() extends EventBus {

    private[this] var consumersRegistry = List[Consumer]()

    override def send(event: Event): Unit = {
      consumersRegistry foreach { f =>
        Future {f(event)}
      }
    }

    override def attach(e: Consumer): Unit = {
      consumersRegistry = e :: consumersRegistry
    }
  }
}

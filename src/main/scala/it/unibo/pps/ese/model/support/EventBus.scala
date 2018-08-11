package it.unibo.pps.ese.model.support

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
        f(event)
      }
    }

    override def attach(e: Consumer): Unit = {
      consumersRegistry = e :: consumersRegistry
    }
  }
}

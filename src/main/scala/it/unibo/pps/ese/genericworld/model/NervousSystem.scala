package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support._

import scala.concurrent.{Future, Promise}

sealed trait NervousSystem {
  def publish(event: Event) : Unit
  def subscribe(handler: Event => Unit)
  def requireData[A <: RequestEvent, B <: ResponseEvent ](request: A): Future[B]
  def addMapping[A <: Event](mapper: (Class[A], A => Seq[EntityProperty]))
}

object NervousSystem {

  def apply(): NervousSystem = new SnifferNervousSystem()

  private class SnifferNervousSystem extends NervousSystem {

    private[this] val _eventBus = EventBus()
    private[this] var _eventsMappings : List[(Class[_], _ => Seq[EntityProperty])]= List empty

    _eventBus attach (ev => {
      val mapper = _eventsMappings find(e => e._1.getName == ev.getClass.getName)
      if (mapper isDefined) mapper get match {
        case (_, map : (Event => Seq[EntityProperty])) => publish(NewState(map(ev)))
        case _ => Unit
      }
    })

    override def publish(event: Event) : Unit = _eventBus send event

    override def subscribe(handler: Event => Unit) : Unit = _eventBus attach handler

    override def addMapping[A <: Event](mapper: (Class[A], A => Seq[EntityProperty])): Unit = _eventsMappings = mapper :: _eventsMappings

    override def requireData[A <: RequestEvent, B <: ResponseEvent](request: A): Future[B] = {
      val result = Promise[B]()
      lazy val consumer : Consumer = {
        case response: B if response.id == request.id =>
          result success response
          _eventBus detach consumer
        case _ => Unit
      }
      _eventBus attach consumer
      _eventBus send request
      result future
    }
  }
}
package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support._

import scala.concurrent.{Future, Promise}

sealed trait NervousSystem {
  def publish(event: Event) : Unit
  def subscribe(handler: Event => Unit)
  def requireData[A <: RequestEvent, B <: ResponseEvent : Manifest](request: A): Future[B]
  def addMapping[A <: Event](mapper: (Class[A], A => Seq[EntityProperty]))
  def notifyOnTasksEnd(): Future[Done]
}

sealed trait ManageableNervousSystem extends NervousSystem {
  def dispose(): Unit
}

object NervousSystem {

  def apply(): ManageableNervousSystem = new SnifferNervousSystem()

  private class SnifferNervousSystem extends ManageableNervousSystem {

    private[this] val _eventBus = EventBus()
    private[this] var _eventsMappings : List[(Class[_], _ => Seq[EntityProperty])]= List empty
    private[this] val _mapper : Consumer = ev => {
      val mapper = _eventsMappings find(e => e._1.getName == ev.getClass.getName)
      if (mapper isDefined) mapper get match {
        case (_, map: (Event => Seq[EntityProperty])) => publish(UpdateEntityState(map(ev)))
        case _ => Unit
      }
    }

    _eventBus attach _mapper

    override def publish(event: Event) : Unit = _eventBus send event

    override def subscribe(handler: Event => Unit) : Unit = _eventBus attach handler

    override def addMapping[A <: Event](mapper: (Class[A], A => Seq[EntityProperty])): Unit = _eventsMappings = mapper :: _eventsMappings

    override def requireData[A <: RequestEvent, B <: ResponseEvent: Manifest](request: A): Future[B] = {
      val result = Promise[B]()
      lazy val consumer : Consumer = {
        case response: B if response.id == request.id =>
          try {
            result success response
          } catch {
            case e: IllegalStateException =>
              throw new IllegalStateException("Problem with message: " + request.toString, e)
          }
          _eventBus detach consumer
        case _ => Unit
      }
      _eventBus attach consumer
      _eventBus send request
      result future
    }

    override def dispose(): Unit = {
      _eventBus detach _mapper
      _eventsMappings = List empty
    }

    override def notifyOnTasksEnd(): Future[Done] = _eventBus notifyOnTasksEnd()
  }
}
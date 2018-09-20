package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support._

import scala.concurrent.{ExecutionContext, Future, Promise}

sealed trait NervousSystem {
  def publish(event: IdentifiedEvent) : Unit
  def subscribe(handler: Consumer)
  def requireData[A <: RequestEvent, B <: ResponseEvent : Manifest](publisher: String, request: A): SupervisedFuture[B]
  def addMapping[A <: Event](mapper: (Class[A], A => Seq[EntityProperty]))
  def notifyOnTasksEnd(): Future[Done]
}

sealed trait ManageableNervousSystem extends NervousSystem {
  def dispose(): Unit
}

object NervousSystem {

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
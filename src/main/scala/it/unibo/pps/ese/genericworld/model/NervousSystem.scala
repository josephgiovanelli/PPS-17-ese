package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

trait Supervisor {
  def computationStarted: () => Unit
  def computationEnded: () => Unit
}

class SupervisedFuture[T](future: Future[T])(implicit supervisor: Supervisor) {

  private val _future: Future[T] = future

  def onComplete(callback: Try[T] => Unit)(implicit executionContext: ExecutionContext): Unit = {
    supervisor.computationStarted()
    future onComplete(_ => {
      callback(future.value.get)
      supervisor.computationEnded()
    })
  }

  def map[S](mapper: T => S)(implicit executionContext: ExecutionContext): SupervisedFuture[S] =
    new SupervisedFuture(future map mapper)

  def flatMap[S](mapper: T => SupervisedFuture[S])(implicit executionContext: ExecutionContext): SupervisedFuture[S] =
    new SupervisedFuture(future flatMap (x => mapper(x)._future))

  def withFilter(p: T => Boolean)(implicit executionContext: ExecutionContext): SupervisedFuture[T] =
    new SupervisedFuture(future withFilter p)

  def foreach[U](p: T => U)(implicit executionContext: ExecutionContext): Unit = {
    supervisor.computationStarted()
    future foreach p
    supervisor.computationEnded()
  }
}

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
      val mapper = _eventsMappings find(e => e._1.getName == ev.getClass.getName)
      if (mapper isDefined) mapper get match {
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
//      var t: Set[B] = Set()
      lazy val consumer : Consumer = IdentifiedConsumer(getClass.getSimpleName, {
        case response: B if response.id == request.id =>
//          try {
//            result success response
//          } catch {
//            case e: IllegalStateException =>
//              throw new IllegalStateException("Problem with message: " + t.contains(response) + "\n" + request + "\n" + t.head + "\n" + response)
//          }
//          t = t + response
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
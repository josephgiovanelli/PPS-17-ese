package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

class SafeFuture[T](future: Future[T])(implicit onCreation: Unit => Unit, afterExecution: Unit => Unit, executionContext: ExecutionContext) {

  private val _future: Future[T] = future

  def onComplete(callback: Try[T] => Unit): Unit = {
    onCreation()
    future onComplete(_ => {
      callback(future.value.get)
      afterExecution()
    })
  }

  def map[S](mapper: T => S): SafeFuture[S] = new SafeFuture(future map mapper)(onCreation, afterExecution, executionContext)

  def flatMap[S](mapper: T => SafeFuture[S]): SafeFuture[S] =
    new SafeFuture(future flatMap (x => mapper(x)._future))(onCreation, afterExecution, executionContext)

  def withFilter(p: T => Boolean): SafeFuture[T] = new SafeFuture(future withFilter p)(onCreation, afterExecution, executionContext)

  def foreach[U](p: T => U): Unit = {
    onCreation()
    future foreach p
    afterExecution()
  }
}

sealed trait NervousSystem {
  def publish(event: Event) : Unit
  def subscribe(handler: Event => Unit)
  def requireData[A <: RequestEvent, B <: ResponseEvent : Manifest](request: A): SafeFuture[B]
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

    override def requireData[A <: RequestEvent, B <: ResponseEvent: Manifest](request: A): SafeFuture[B] = {
      val result = Promise[B]()
//      var t: Set[B] = Set()
      lazy val consumer : Consumer = {
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
      }
      _eventBus attach consumer
      _eventBus send request

      implicit val onCreation: Unit => Unit = Unit => _eventBus notifyNewTaskStart()
      implicit val afterExecution: Unit => Unit = Unit => _eventBus notifyNewTaskEnd()
      new SafeFuture(result.future)(onCreation, afterExecution, executionContext)
    }

    override def dispose(): Unit = {
      _eventBus detach _mapper
      _eventsMappings = List empty
    }

    override def notifyOnTasksEnd(): Future[Done] = _eventBus notifyOnTasksEnd()
  }
}
package it.unibo.pps.ese.genericworld.model.support

import scala.concurrent.{ExecutionContext, Future}
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
package it.unibo.pps.ese.controller.simulation.runner.core.support

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * This trait must be implemented to give the custom behavior to the SupervisedFuture below
  */
trait Supervisor {
  /**
    * Specify some custom logic to be executed when the SupervisedFuture enters in a onComplete or a forEach block
    * @return
    */
  def computationStarted: () => Unit

  /**
    * Specify some custom logic to be executed when the SupervisedFuture completes a onComplete or a forEach block
    * @return
    */
  def computationEnded: () => Unit
}

/**
  * This custom class enrich the functionalities of a normal Future, allowing
  * a supervisor to track the Future's execution steps.
  * Most of the methods of a normal Future are not present in this wrapper. Have been kept only the main ones,
  * responsible for the monadic behaviour of the class.
  * @param future The enriched future
  * @param supervisor The custom supervisor
  * @tparam T The future type
  */
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
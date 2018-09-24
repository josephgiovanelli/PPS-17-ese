package it.unibo.pps.ese.controller.simulation.runner.core.support

import scala.concurrent.{ExecutionContext, Future}

object FutureHelper {

  /**
    * This class enriches the functionalities of Future's companion object
    */
  implicit class PimpedFuture(companion: Future.type) {

    /**
      * Serialize the execution of a a Future task list
      * @param l The input iterable from wich futures are generated
      * @param fn The mapping function from input to Future
      * @param context The execution context
      * @tparam A The input type
      * @tparam B The output type
      * @return A single future containing the output list
      */
    def serializeFutures[A, B](l: Iterable[A])(fn: A => Future[B])
                                      (implicit context: ExecutionContext): Future[List[B]] =
      l.foldLeft(Future(List.empty[B])) {
        (previousFuture, next) =>
          for {
            previousResults <- previousFuture
            next <- fn(next)
          } yield previousResults :+ next
      }
  }
}



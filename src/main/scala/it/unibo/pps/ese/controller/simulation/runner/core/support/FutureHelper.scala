package it.unibo.pps.ese.controller.simulation.runner.core.support

import scala.concurrent.{ExecutionContext, Future}

object FutureHelper {
  implicit class PimpedFuture(companion: Future.type) {
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



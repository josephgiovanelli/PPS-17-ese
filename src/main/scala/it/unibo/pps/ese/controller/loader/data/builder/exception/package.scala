package it.unibo.pps.ese.controller.loader.data.builder

package object exception {
  implicit class OptionExceptionMixing(exception: CompleteBuildException) {
    def ++:(opt: Option[CompleteBuildException]): Option[CompleteBuildException] = opt match {
      case Some(exc: CompleteBuildException) =>
        Some(new CompleteBuildException(exc.problems ++ exception.problems:_*))
      case None =>
        Some(exception)
    }

    def :++(opt: Option[CompleteBuildException]): Option[CompleteBuildException] = opt match {
      case Some(exc: CompleteBuildException) =>
        Some(new CompleteBuildException(exception.problems ++ exc.problems:_*))
      case None =>
        Some(exception)
    }
  }
}

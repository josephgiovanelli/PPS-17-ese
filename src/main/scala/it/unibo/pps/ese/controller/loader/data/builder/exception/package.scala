package it.unibo.pps.ese.controller.loader.data.builder

package object exception {
  implicit class OptionExceptionMixing(exception: CompleteBuildException) {
    def ++:(opt: Option[CompleteBuildException]): Option[CompleteBuildException] = opt match {
      case Some(exc: CompleteBuildException) =>
        Some(exc +: exception)
      case None =>
        Some(exception)
    }

    def :++(opt: Option[CompleteBuildException]): Option[CompleteBuildException] = opt match {
      case Some(exc: CompleteBuildException) =>
        Some(exception :+ exc)
      case None =>
        Some(exception)
    }
  }
}

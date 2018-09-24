package it.unibo.pps.ese.controller.simulation.loader.data.builder

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

  implicit class OptionExceptionOptMixing(exception: Option[CompleteBuildException]) {
    def ++:(opt: Option[CompleteBuildException]): Option[CompleteBuildException] = (exception, opt) match {
      case (Some(subexc), Some(objexc)) =>
        Some(objexc +: subexc)
      case (r: Some[CompleteBuildException], None) =>
        r
      case (None, r: Some[CompleteBuildException]) =>
        r
      case _ =>
        None
    }

    def :++(opt: Option[CompleteBuildException]): Option[CompleteBuildException] = (exception, opt) match {
      case (Some(subexc), Some(objexc)) =>
        Some(subexc :+ objexc)
      case (r: Some[CompleteBuildException], None) =>
        r
      case (None, r: Some[CompleteBuildException]) =>
        r
      case _ =>
        None
    }
  }
}

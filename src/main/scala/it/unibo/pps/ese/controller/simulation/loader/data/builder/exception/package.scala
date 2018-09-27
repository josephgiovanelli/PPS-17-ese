package it.unibo.pps.ese.controller.simulation.loader.data.builder

package object exception {

  /** implicit class that add to [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
    * possibility to be mixed with  an option of exception
    *
    * @param exception CompleteBuildException
    */
  implicit class OptionExceptionMixing(exception: CompleteBuildException) {
    /** Right associative operator to mix exception with an option of [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
      *
      * @param opt Exception option to mix
      * @return Option containing mixed exception
      */
    def ++:(opt: Option[CompleteBuildException]): Option[CompleteBuildException] = opt match {
      case Some(exc: CompleteBuildException) =>
        Some(exc +: exception)
      case None =>
        Some(exception)
    }
    /** Left associative operator to mix exception with an option of [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
      *
      * @param opt Exception  option to mix
      * @return Option containing mixed exception
      */
    def :++(opt: Option[CompleteBuildException]): Option[CompleteBuildException] = opt match {
      case Some(exc: CompleteBuildException) =>
        Some(exception :+ exc)
      case None =>
        Some(exception)
    }
  }

  /** implicit class that add to an Option of
    * [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
    * possibility to be mixed with  an option of exception
    *
    * @param exception CompleteBuildException option
    */
  implicit class OptionExceptionOptMixing(exception: Option[CompleteBuildException]) {
    /** Right associative operator to mix exception option with an option of [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
      *
      * @param opt Exception option to mix
      * @return Option containing mixed exception
      */
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
    /** Left associative operator to mix exception option with an option of [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
      *
      * @param opt Exception option to mix
      * @return Option containing mixed exception
      */
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

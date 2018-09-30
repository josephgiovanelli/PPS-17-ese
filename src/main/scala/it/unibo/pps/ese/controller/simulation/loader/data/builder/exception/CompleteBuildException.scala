package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

/** Specialization of [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.HierarchyMotivationsException]]
  * that indicated a problem in a builder's complete build
  */
trait CompleteBuildException extends HierarchyMotivationsException {
  override type MixedException = CompleteBuildException

  def +:(exception: HierarchyMotivationsException): MixedException = {
    CompleteBuildException(exception.motivations ++ motivations)
  }

  def :+(exception: HierarchyMotivationsException): MixedException = {
    CompleteBuildException(motivations ++ exception.motivations)
  }
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]*/
object CompleteBuildException {

  /**
    * @param motivations Motivations
    * @return New [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
    */
  def apply(motivations: Iterable[Motivation]): CompleteBuildException = new AbsHierarchyMotivationsException(motivations) with CompleteBuildException

  /**
    * @param motivation Motivation as string
    * @param subMotivations Sub motivations
    * @return New [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
    */
  def apply(motivation: String, subMotivations: Iterable[Motivation] = Seq()): CompleteBuildException = {
    new AbsHierarchyMotivationsException(motivation, subMotivations) with CompleteBuildException
  }

  /**
    * @param motivation Motivation as string
    * @param exc Exceptions that indicates sub motivations
    * @return New [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]]
    */
  def apply(motivation: String, exc: Iterable[HierarchyMotivationsException])(implicit i: DummyImplicit): CompleteBuildException = {
    new AbsHierarchyMotivationsException(motivation, exc) with CompleteBuildException
  }

}

package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

trait CompleteBuildException extends HierarchyMotivationsException {
  override type MixedException = CompleteBuildException

  def +:(exception: HierarchyMotivationsException): MixedException = {
    CompleteBuildException(exception.motivations ++ motivations)
  }

  def :+(exception: HierarchyMotivationsException): MixedException = {
    CompleteBuildException(motivations ++ exception.motivations)
  }
}

object CompleteBuildException {

  def apply(motivations: Iterable[Motivation]): CompleteBuildException = new AbsHierarchyMotivationsException(motivations) with CompleteBuildException

  def apply(motivation: String, subMotivations: Iterable[Motivation] = Seq()): CompleteBuildException = {
    new AbsHierarchyMotivationsException(motivation, subMotivations) with CompleteBuildException
  }

  def apply(motivation: String, exc: Iterable[HierarchyMotivationsException])(implicit i: DummyImplicit): CompleteBuildException = {
    new AbsHierarchyMotivationsException(motivation, exc) with CompleteBuildException
  }

}

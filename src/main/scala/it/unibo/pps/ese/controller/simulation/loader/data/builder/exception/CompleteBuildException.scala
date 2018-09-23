package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

trait HierarchyMotivationsException extends Exception {
  type MixedException <: HierarchyMotivationsException
  def motivations: Iterable[Motivation]
  def +:(exception: HierarchyMotivationsException): MixedException
  def :+(exception: HierarchyMotivationsException): MixedException
}

abstract class AbsHierarchyMotivationsException(val motivations: Iterable[Motivation]) extends Exception(toString) with HierarchyMotivationsException {

  def this(motivation: String, subMotivations: Iterable[Motivation] = Seq()) {
    this(Seq(new Motivation(motivation, subMotivations)))
  }

  def this(motivation: String, exc: Iterable[HierarchyMotivationsException])(implicit i: DummyImplicit) {
    this(motivation, exc.flatMap(_.motivations))
  }
  override def toString: String = motivations.mkString("\n")
}

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

class Motivation(val motivation: String, subMotivations: Iterable[Motivation]) {
  override def toString: String = {
    motivation + subMotivations.foldLeft("")((a, b) => a + "\n  " + b.toString.replaceAll("\n", "\n  "))
  }
}

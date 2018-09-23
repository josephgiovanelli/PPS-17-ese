package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

trait HierarchyMotivationsException extends Exception {
  type MixedException <: HierarchyMotivationsException
  def motivations: Iterable[Motivation]
  def +:(exception: HierarchyMotivationsException): MixedException
  def :+(exception: HierarchyMotivationsException): MixedException
}

abstract class AbsHierarchyMotivationsException(val motivations: Iterable[Motivation]) extends Exception(toString)
  with HierarchyMotivationsException {

  def this(motivation: String, subMotivations: Iterable[Motivation] = Seq()) {
    this(Seq(new Motivation(motivation, subMotivations)))
  }

  def this(motivation: String, exc: Iterable[HierarchyMotivationsException])(implicit i: DummyImplicit) {
    this(motivation, exc.flatMap(_.motivations))
  }
  override def toString: String = motivations.mkString("\n")
}

class Motivation(val motivation: String, subMotivations: Iterable[Motivation]) {
  override def toString: String = {
    motivation + subMotivations.foldLeft("")((a, b) => a + "\n  " + b.toString.replaceAll("\n", "\n  "))
  }
}
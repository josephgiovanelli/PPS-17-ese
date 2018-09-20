package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

class CompleteBuildException(val motivations: Iterable[Motivation]) extends Exception(toString) {

  def this(motivation: String, subMotivations: Iterable[Motivation] = Seq()) {
    this(Seq(new Motivation(motivation, subMotivations)))
  }

  def this(motivation: String, exc: Iterable[CompleteBuildException])(implicit i: DummyImplicit) {
    this(motivation, exc.flatMap(_.motivations))
  }

  def +:(exception: CompleteBuildException): CompleteBuildException = {
    new CompleteBuildException(exception.motivations ++ motivations)
  }

  def :+(exception: CompleteBuildException): CompleteBuildException = {
    new CompleteBuildException(motivations ++ exception.motivations)
  }

  override def toString: String = motivations.mkString("\n")

}

class Motivation(val motivation: String, subMotivations: Iterable[Motivation]) {
  override def toString: String = {
    motivation + subMotivations.foldLeft("")((a, b) => a + "\n  " + b.toString.replaceAll("\n", "\n  "))
  }
}

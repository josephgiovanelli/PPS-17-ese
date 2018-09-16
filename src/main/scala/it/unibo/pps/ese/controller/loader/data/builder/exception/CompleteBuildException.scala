package it.unibo.pps.ese.controller.loader.data.builder.exception

class CompleteBuildException(val motivations: Iterable[Motivation]) extends Exception() {

  def this(motivation: String, subMotivations: Iterable[Motivation] = Seq()) {
    this(Seq(new Motivation(motivation, subMotivations)))
  }

  def this(motivation: String, exc: Iterable[CompleteBuildException])(implicit i: DummyImplicit) {
    this(motivation, exc.map(e => new Motivation("---", e.motivations)))
  }

  def +:(exception: CompleteBuildException): CompleteBuildException = {
    new CompleteBuildException(exception.motivations ++ motivations)
  }

  def :+(exception: CompleteBuildException): CompleteBuildException = {
    new CompleteBuildException(motivations ++ exception.motivations)
  }
  
}

class Motivation(motivation: String, subMotivations: Iterable[Motivation])

package it.unibo.pps.ese.entitybehaviors.decisionsupport.prologimplementation

object Scala2P {
  import alice.tuprolog._

  def extractTerm(t: Term, i: Integer): Term =
    t.asInstanceOf[Struct].getArg(i).getTerm

  implicit def stringToTerm(s: String): Term = Term.createTerm(s)
  implicit def IntToTerm(i: scala.Int): Term = new Int(i)

  val engine = new Prolog

  def mkPrologTheory(clauses: String*) =
    engine.setTheory(new Theory(clauses mkString " "))

  def modifyDynamicKnowledge(clause: String): SolveInfo =
    engine.solve(clause + ".")

  def createEngine: Term => Stream[Term] = {
    goal => {
      new Iterable[Term] {
        override def iterator = new Iterator[Term] {
          var solution = engine.solve(goal);

          override def hasNext = solution.isSuccess ||
            solution.hasOpenAlternatives

          override def next() =
            try solution.getSolution finally solution = engine.solveNext()
        }
      }.toStream
    }
  }

}

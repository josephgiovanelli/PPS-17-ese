package it.unibo.pps.ese.model.components.animals.brain.decisionsupport.prologimplementation

/**
  * It allows the loading of the theory, the interrogation of the prolog engine and the dynamic addition of knowledge.
  */
object Scala2P {
  import alice.tuprolog._

  def extractTerm(t: Term, i: Integer): Term =
    t.asInstanceOf[Struct].getArg(i).getTerm

  implicit def stringToTerm(s: String): Term = Term.createTerm(s)
  implicit def IntToTerm(i: scala.Int): Term = new Int(i)

  val engine = new Prolog

  def mkPrologTheory(clauses: String*): Unit =
    engine.setTheory(new Theory(clauses mkString " "))

  def modifyDynamicKnowledge(clause: String): SolveInfo =
    engine.solve(clause + ".")

  def createEngine: Term => Stream[Term] = {
    goal => {
      new Iterable[Term] {
        override def iterator: Iterator[Term] = new Iterator[Term] {
          var solution: SolveInfo = engine.solve(goal)

          override def hasNext: Boolean = solution.isSuccess ||
            solution.hasOpenAlternatives

          override def next(): Term =
            try solution.getSolution finally solution = engine.solveNext()
        }
      }.toStream
    }
  }

}

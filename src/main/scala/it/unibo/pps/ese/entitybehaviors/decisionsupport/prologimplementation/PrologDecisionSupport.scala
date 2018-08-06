package it.unibo.pps.ese.entitybehaviors.decisionsupport.prologimplementation

import it.unibo.pps.ese.entitybehaviors.decisionsupport.{DecisionSupport, EntityAttributes}

import scala.io.Source

object PrologDecisionSupport {
  def apply(): DecisionSupport = new PrologDecisionSupportImpl()

  private class PrologDecisionSupportImpl() extends DecisionSupport {
    import Scala2P._
    import alice.tuprolog._

    val engine: Term => Stream[Term] = createEngine
    val filename: String = getClass.getResource("/DecisionTree.pl").getPath
    val fileContents: String = Source.fromFile(filename).getLines.reduce((line1, line2) => line1 + "\n" + line2)
    mkPrologTheory(fileContents)

    override def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit =
      entitiesAttributes foreach (entityAttributes => modifyDynamicKnowledge("add" + entityAttributes))

    override def clearVisualField(): Unit =
      engine(new Struct("deleteEntity", new Var()))

    override def discoverPreys(hunter: EntityAttributes): Stream[(Term, Term)] =
      engine(new Struct("discoverPreys", hunter.name.toString, new Var(), new Var())) map (x => (extractTerm(x, 1), extractTerm(x, 2)))

    override def discoverPartners(entity: EntityAttributes): Stream[(Term, Term)] =
      engine(new Struct("discoverPartners", entity.name.toString, new Var(), new Var())) map (x => (extractTerm(x, 1), extractTerm(x, 2)))
  }

}
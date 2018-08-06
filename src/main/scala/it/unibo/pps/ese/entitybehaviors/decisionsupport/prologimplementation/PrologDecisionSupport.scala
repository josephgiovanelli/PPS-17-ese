package it.unibo.pps.ese.entitybehaviors.decisionsupport.prologimplementation

import it.unibo.pps.ese.entitybehaviors.decisionsupport.{DecisionSupport, EntityAttributes}

import scala.io.Source

object PrologDecisionSupport {
  def apply(): DecisionSupport = new PrologDecisionSupportImpl()

  private class PrologDecisionSupportImpl() extends DecisionSupport {
    import Scala2P._
    import alice.tuprolog._

    implicit def termToInt(term: Term): scala.Int = term.toString.toInt
    implicit def tupleTermToTupleInt(tuple2: (Term, Term)): (scala.Int, scala.Int) = (tuple2._1, tuple2._2)
    implicit def streamTupleTermToStreamTupleInt(streamTupleTerm: Stream[(Term, Term)]): Stream[(scala.Int, scala.Int)] =
      streamTupleTerm map tupleTermToTupleInt


    val engine: Term => Stream[Term] = createEngine
    val filename: String = getClass.getResource("/DecisionTree.pl").getPath
    val fileContents: String = Source.fromFile(filename).getLines.reduce((line1, line2) => line1 + "\n" + line2)
    mkPrologTheory(fileContents)

    override def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit =
      entitiesAttributes foreach (entityAttributes => modifyDynamicKnowledge("add" + entityAttributes))

    override def clearVisualField(): Unit =
      engine(new Struct("deleteEntity", new Var()))

    override def discoverPreys(hunter: EntityAttributes): Stream[(scala.Int, scala.Int)] = {
      val discoverPreys = new Struct("discoverPreys", hunter.name.toString, new Var("Y"), new Var("Lenght"))
      engine(discoverPreys) map (x => (extractTerm(x, 1), extractTerm(x, 2)))
    }

    override def discoverPartners(entity: EntityAttributes): Stream[(scala.Int, scala.Int)] = {
      val discoverPartners = new Struct("discoverPartners", entity.name.toString, new Var("Y"), new Var("Lenght"))
      engine(discoverPartners) map (x => (extractTerm(x, 1), extractTerm(x, 2)))
    }

    override def nextMove(from: EntityAttributes, to: EntityAttributes): (scala.Int, scala.Int) = {
      val nextMove = new Struct("nextMove", from.name.toString, to.name.toString, new Var("NewX"), new Var("NewY"))
      engine(nextMove) map (x => (extractTerm(x, 2), extractTerm(x, 3))) head
    }
  }

}
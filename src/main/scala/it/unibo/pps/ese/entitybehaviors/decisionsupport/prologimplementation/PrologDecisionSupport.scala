package it.unibo.pps.ese.entitybehaviors.decisionsupport.prologimplementation

import it.unibo.pps.ese.entitybehaviors.decisionsupport.Point.Point
import it.unibo.pps.ese.entitybehaviors.decisionsupport._

import scala.io.Source

object PrologDecisionSupport {
  def apply(worldRules: WorldRules): DecisionSupport = new PrologDecisionSupportImpl(worldRules)

  private class PrologDecisionSupportImpl(worldRules: WorldRules) extends AbstractDecisionSupport(worldRules) {
    import Scala2P._
    import alice.tuprolog._

    implicit def termToInt(term: Term): scala.Int = term.toString.toInt
    implicit def tupleTermToTupleInt(tuple: (Term, Term)): (scala.Int, scala.Int) = (tuple._1, tuple._2)
    implicit def streamTupleTermToStreamTupleInt(streamTupleTerm: Stream[(Term, Term)]): Stream[(scala.Int, scala.Int)] = streamTupleTerm map tupleTermToTupleInt
    implicit def tupleTermToEntityChoice(tuple: (Term, Term)): EntityChoice = EntityChoice(tuple._1, tuple._2)
    implicit def streamTupleTermStreamEntityChoice(tuples: Stream[(Term, Term)]): Stream[EntityChoice] = tuples map tupleTermToEntityChoice
    implicit def tupleTermToPoint(tuple: (Term, Term)): Point = Point(termToInt(tuple._1), termToInt(tuple._2))


    val engine: Term => Stream[Term] = createEngine
    val filename: String = getClass.getResource("/DecisionTree.pl").getPath
    val fileContents: String = Source.fromFile(filename).getLines.reduce((line1, line2) => line1 + "\n" + line2)
    mkPrologTheory(fileContents)
    modifyDynamicKnowledge("setAttackThreshold(" + worldRules.attackThreshold + ")")
    modifyDynamicKnowledge("setHeightThresholds(" + worldRules.heightThresholds._1 + "," + worldRules.heightThresholds._2 + ")")
    worldRules.compatibleHuntingKinds foreach (compatibleKind => modifyDynamicKnowledge("addCompatibleHuntingKinds(" + compatibleKind._1 + "," + compatibleKind._2 + ")"))
    worldRules.compatibleCouplingKinds foreach (compatibleKind => modifyDynamicKnowledge("addCompatibleCouplingKinds(" + compatibleKind._1 + "," + compatibleKind._2 + ")"))


    override def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit =
      entitiesAttributes foreach (entityAttributes => modifyDynamicKnowledge("add" + entityAttributes))

    override def clearVisualField(): Unit =
      engine(new Struct("deleteEntity", new Var()))

    override def discoverPreys(hunter: EntityAttributes): Stream[EntityChoice] = {
      val discoverPreys = new Struct("discoverPreys", hunter.name.toString, new Var("Y"), new Var("Length"))
      engine(discoverPreys) map (x => (extractTerm(x, 1), extractTerm(x, 2)))
    }

    override def discoverPartners(entity: EntityAttributes): Stream[EntityChoice] = {
      val discoverPartners = new Struct("discoverPartners", entity.name.toString, new Var("Y"), new Var("Length"))
      engine(discoverPartners) map (x => (extractTerm(x, 1), extractTerm(x, 2)))
    }

    override def nextMove(from: EntityAttributes, to: EntityAttributes): Point = {
      val nextMove = new Struct("nextMove", from.name.toString, to.name.toString, new Var("NewX"), new Var("NewY"))
      engine(nextMove) map (x => (extractTerm(x, 2), extractTerm(x, 3))) head
    }
  }

}
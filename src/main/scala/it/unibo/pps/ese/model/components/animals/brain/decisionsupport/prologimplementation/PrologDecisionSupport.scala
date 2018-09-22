package it.unibo.pps.ese.model.components.animals.brain.decisionsupport.prologimplementation

import it.unibo.pps.ese.model.components.animals.brain.decisionsupport._

import scala.io.Source

trait WorldRulesListener {
  def updateCouplingKind(set: Set[(String, String)]): Unit
  def updateHuntingKind(set: Set[(String, String)]): Unit
}

object PrologDecisionSupport {
  def apply(): DecisionSupport = new PrologDecisionSupportImpl()

  private class PrologDecisionSupportImpl() extends DecisionSupport with WorldRulesListener{

    import Scala2P._
    import alice.tuprolog._

    implicit def termToInt(term: Term): scala.Int = term.toString.toInt
    implicit def termToString(term: Term): String = term.toString
    implicit def tupleTermToTuple2(tuple: (Term, Term)): (String, scala.Int) = (tuple._1, tuple._2)
    implicit def tupleTermToTupleInt(tuple: (Term, Term)): (scala.Int, scala.Int) = (tuple._1, tuple._2)
    implicit def streamTupleTermToStreamTuple2(streamTupleTerm: Stream[(Term, Term)]): Stream[(String, scala.Int)] = streamTupleTerm map tupleTermToTuple2
    implicit def streamTupleTermToStreamTupleInt(streamTupleTerm: Stream[(Term, Term)]): Stream[(scala.Int, scala.Int)] = streamTupleTerm map tupleTermToTupleInt
    implicit def tupleTermToEntityChoice(tuple: (Term, Term)): EntityChoice = new EntityChoice(tuple._1, tuple._2)
    implicit def streamTupleTermStreamEntityChoice(tuples: Stream[(Term, Term)]): Stream[EntityChoice] = tuples map tupleTermToEntityChoice
    implicit def tupleTermToPoint(tuple: (Term, Term)): GeneralPosition[scala.Int] = GeneralPosition(tuple._1, tuple._2)
    implicit def streamTupleTermStreamGeneralPosition(tuples: Stream[(Term, Term)]): Stream[GeneralPosition[scala.Int]] = tuples map tupleTermToPoint


    val engine: Term => Stream[Term] = createEngine
    val filename: String = getClass.getResource("/it/unibo/pps/ese/model/components/animals/brain/decisionsupport/prologimplementation/DecisionTree.pl").getPath
    val fileContents: String = Source.fromFile(filename).getLines.reduce((line1, line2) => line1 + "\n" + line2)
    mkPrologTheory(fileContents)
    modifyDynamicKnowledge("setAttackThreshold(" + worldRules.attackThreshold + ")")
    modifyDynamicKnowledge("setAttractivenessThreshold(" + worldRules.couplingThreshold + ")")
    modifyDynamicKnowledge("setHeightThresholds(" + worldRules.heightThresholds + ")")
    worldRules.compatibleHuntingKinds foreach (compatibleKind => modifyDynamicKnowledge("addCompatibleHuntingKinds(" + compatibleKind._1 + "," + compatibleKind._2 + ")"))
    worldRules.compatibleCouplingKinds foreach (compatibleKind => modifyDynamicKnowledge("addCompatibleCouplingKinds(" + compatibleKind._1 + "," + compatibleKind._2 + ")"))
    worldRules.addListener(this)

    var supportMap: Map[String, String] = Map.empty

    object PrologEntityAttributesImpl {
      def apply(newName: String, entityAttributesImpl: EntityAttributesImpl): EntityAttributesImpl =
        EntityAttributesImpl(newName, entityAttributesImpl.kind, entityAttributesImpl.height, entityAttributesImpl.strength, entityAttributesImpl.defense, entityAttributesImpl.position, entityAttributesImpl.attractiveness, entityAttributesImpl.gender)
    }

    override def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit = {
      supportMap = entitiesAttributes.zipWithIndex.map(x => (x._1.name, x._2.toString)).toMap
      supportMap ++= supportMap.map(_.swap)
      entitiesAttributes foreach (entityAttributes =>
        modifyDynamicKnowledge("add" + PrologEntityAttributesImpl(supportMap(entityAttributes.name), entityAttributes)))
    }

    override def clearVisualField(): Unit = {
      supportMap.foreach(x => engine(new Struct("deleteEntity", new Var())))
      supportMap = Map.empty
    }

    override def discoverPreys(hunter: EntityAttributes): Stream[EntityChoice] = {
      var result: Stream[EntityChoice] = Stream.empty
      if(supportMap.get(hunter.name).isDefined) {
        val discoverPreys = new Struct("discoverPreys", supportMap(hunter.name), new Var("Y"), new Var("Length"))
        result = engine(discoverPreys) map (x => (extractTerm(x, 1), extractTerm(x, 2)))
      }
      result.map(x => (supportMap(x.name), x.distance))
    }

    override def discoverPartners(entity: EntityAttributes): Stream[EntityChoice] = {
      var result: Stream[EntityChoice] = Stream.empty
      if(supportMap.get(entity.name).isDefined) {
        val discoverPartners = new Struct("discoverPartners", supportMap(entity.name), new Var("Y"), new Var("Length"))
        result = engine(discoverPartners) map (x => (extractTerm(x, 1), extractTerm(x, 2)))
      }
      result.map(x => (supportMap(x.name), x.distance))
    }

    override def nextMove(from: EntityAttributes, to: EntityAttributes): GeneralPosition[scala.Int] = {
      val nextMove = new Struct("nextMove", supportMap(from.name), supportMap(to.name), new Var("NewX"), new Var("NewY"))
      val result: Stream[GeneralPosition[scala.Int]] = engine(nextMove) map (x => (extractTerm(x, 2), extractTerm(x, 3)))
      if (result isEmpty) from.position else result head
    }

    override def updateCouplingKind(set: Set[(String, String)]): Unit = {
      set foreach (compatibleKind => {
        modifyDynamicKnowledge("deleteCompatibleCouplingKinds(" + compatibleKind._1 + "," + compatibleKind._2 + ")")
        modifyDynamicKnowledge("addCompatibleCouplingKinds(" + compatibleKind._1 + "," + compatibleKind._2 + ")")
      })
    }

    override def updateHuntingKind(set: Set[(String, String)]): Unit = {
      set foreach (compatibleKind => {
        modifyDynamicKnowledge("deleteCompatibleHuntingKinds(" + compatibleKind._1 + "," + compatibleKind._2 + ")")
        modifyDynamicKnowledge("addCompatibleHuntingKinds(" + compatibleKind._1 + "," + compatibleKind._2 + ")")
      })
    }

  }

}
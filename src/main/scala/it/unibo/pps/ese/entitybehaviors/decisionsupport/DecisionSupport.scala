package it.unibo.pps.ese.entitybehaviors.decisionsupport

import alice.tuprolog.{Term}

trait DecisionSupport {
  def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit
  def clearVisualField(): Unit
  def discoverPreys(hunter: EntityAttributes): Stream[(Int, Int)]
  def discoverPartners(entity: EntityAttributes): Stream[(Int, Int)]
  def nextMove(from: EntityAttributes, to: EntityAttributes): (Int, Int)

}

object DecisionSupport {
  def apply(): DecisionSupport = new DecisionSupportImpl()

  private class DecisionSupportImpl() extends DecisionSupport {

    var visualField: Set[EntityAttributes] = Set.empty

    override def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit = visualField ++= entitiesAttributes

    override def clearVisualField(): Unit = visualField = Set.empty

    override def discoverPreys(hunter: EntityAttributes): Stream[(Int, Int)] = ???

    override def discoverPartners(entity: EntityAttributes): Stream[(Int, Int)] = ???

    override def nextMove(from: EntityAttributes, to: EntityAttributes): (Int, Int) = ???
  }

}


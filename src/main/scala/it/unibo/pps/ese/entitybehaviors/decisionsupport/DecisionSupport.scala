package it.unibo.pps.ese.entitybehaviors.decisionsupport

import alice.tuprolog.Term

trait DecisionSupport {
  def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit
  def clearVisualField(): Unit
  def discoverPreys(hunter: EntityAttributes): Stream[(Term, Term)]
  def discoverPartners(entity: EntityAttributes): Stream[(Term, Term)]
}

object EntityKinds extends Enumeration {
  val carnivorous, herbivore, plant = Value
}

case class EntityAttributes(name: Int, kind: EntityKinds.Value, height: Int, strong: Int, defense: Int, position: (Int, Int)) {
  override def toString: String = "Entity(" + name + ", " + kind + ", " + height + ", " + strong + ", " + defense + ", [" + position._1 + ", " + position._2 + "])"
}


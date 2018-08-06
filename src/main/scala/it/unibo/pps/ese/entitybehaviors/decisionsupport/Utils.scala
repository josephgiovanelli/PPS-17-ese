package it.unibo.pps.ese.entitybehaviors.decisionsupport

object EntityKinds extends Enumeration {
  val carnivorous, herbivore, plant = Value
}

case class EntityAttributes(name: Int, kind: EntityKinds.Value, height: Int, strong: Int, defense: Int, position: (Int, Int)) {
  override def toString: String = "Entity(" + name + ", " + kind + ", " + height + ", " + strong + ", " + defense + ", [" + position._1 + ", " + position._2 + "])"
}
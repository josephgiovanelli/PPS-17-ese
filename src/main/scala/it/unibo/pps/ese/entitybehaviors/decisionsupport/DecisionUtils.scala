package it.unibo.pps.ese.entitybehaviors.decisionsupport

import it.unibo.pps.ese.entitybehaviors.StaticRules
import it.unibo.pps.ese.entitybehaviors.decisionsupport.Point.Point

/*object EntityKinds extends Enumeration {
  val carnivorous, herbivore, plant = Value
}*/


object EntityKinds extends Enumeration {
  type EntityKinds = Value
  val entityKinds: Set[String] = StaticRules.instance().getValues()
  entityKinds.foreach(Value)

  private val constants: Map[Symbol, EntityKinds.Value] = entityKinds.map(v => Symbol(v) -> withName(v)).toMap
  def apply(c: Symbol): EntityKinds = constants(c)
  def unapply(arg: EntityKinds): Option[Symbol] = Some(Symbol(values.find(x => arg.equals(x)).get.toString))
}

abstract class GeneralPosition[PositionMeasure](val x: PositionMeasure, val y: PositionMeasure) {
  //implicit def generalPositionToTuple(generalPosition: GeneralPosition[PositionMeasure]): (PositionMeasure, PositionMeasure) = (generalPosition.x, generalPosition.y)
  //implicit def tupleToGeneralPosition(tuple: (PositionMeasure, PositionMeasure)): GeneralPosition[PositionMeasure]

  def sameAbscissa(generalPosition: GeneralPosition[PositionMeasure]): Int
  def sameOrdinate(generalPosition: GeneralPosition[PositionMeasure]): Int
}

object Point {
  implicit def tupleToPoint(tuple: (Int, Int)): Point = Point(tuple._1, tuple._2)
  implicit def pointToTuple(point: Point): (Int, Int) = (point.x, point.y)

  case class Point(override val x: Int, override val y: Int) extends GeneralPosition[Int](x: Int, y: Int) {
    override def sameAbscissa(generalPosition: GeneralPosition[Int]): Int = if (x == generalPosition.x) 0 else if (x > generalPosition.x) 1 else -1
    override def sameOrdinate(generalPosition: GeneralPosition[Int]): Int = if (y == generalPosition.y) 0 else if (y > generalPosition.y) 1 else -1
  }
}

/*trait EntityAttributesTypes {
  type Name
  type Kind
  type HeightMeasure
  type AttackMeasure
  type Position[PositionMeasure] <: GeneralPosition[PositionMeasure]
}


abstract class AbstractEntityAttributes[PositionMeasure]() extends EntityAttributesTypes {
  val name: Name
  val kind: Kind
  val height: HeightMeasure
  val strong: AttackMeasure
  val defense: AttackMeasure
  val position: Position[PositionMeasure]
}*/

case class EntityAttributes(name: Int, kind: EntityKinds.Value, height: Int, strong: Int, defense: Int, position: Point) {
  override def toString: String = "Entity(" + name + ", " + kind + ", " + height + ", " + strong + ", " + defense + ", [" + position.x + ", " + position.y + "])"
}

case class EntityChoice(name: Int, distance: Int)


object WorldRules {
  def apply(attackThreshold: Int, heightThresholds: (Int, Int), compatibleHuntingKinds: Set[(EntityKinds.Value, EntityKinds.Value)], compatibleCouplingKinds: Set[(EntityKinds.Value, EntityKinds.Value)]): WorldRules =  WorldRules(attackThreshold, heightThresholds, compatibleHuntingKinds, compatibleCouplingKinds)
  implicit def stringToEntityKinds(string: String): EntityKinds.Value = EntityKinds(Symbol(string))
  implicit def tupleStringToEntityKinds(tuple: (String, String)): (EntityKinds.Value, EntityKinds.Value) = (tuple._1, tuple._2)
  implicit def setTupleStringToSetTupleEntityKinds(set: Set[(String, String)]): Set[(EntityKinds.Value, EntityKinds.Value)] = set map tupleStringToEntityKinds

  case class WorldRules(attackThreshold: Int, heightThresholds: (Int, Int), compatibleHuntingKinds: Set[(EntityKinds.Value, EntityKinds.Value)], compatibleCouplingKinds: Set[(EntityKinds.Value, EntityKinds.Value)])
}



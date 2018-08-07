package it.unibo.pps.ese.entitybehaviors.decisionsupport

import it.unibo.pps.ese.entitybehaviors.decisionsupport.Point.Point

object EntityKinds extends Enumeration {
  val carnivorous, herbivore, plant = Value
}

case class EntityAttributes(name: Int, kind: EntityKinds.Value, height: Int, strong: Int, defense: Int, position: Point) {
  override def toString: String = "Entity(" + name + ", " + kind + ", " + height + ", " + strong + ", " + defense + ", [" + position.x + ", " + position.y + "])"
}

case class EntityChoice(name: Int, distance: Int)


object Point {
  def apply(x: Int, y: Int): Point = Point(x, y)

  implicit def tupleToPoint(tuple: (Int, Int)): Point = Point(tuple._1, tuple._2)
  implicit def pointToTuple(point: Point): (Int, Int) = (point.x, point.y)

  case class Point(x: Int, y: Int) {
    def sameAbscissa(point: Point): Int = if (x == point.x) 0 else if (x > point.x) 1 else -1
    def sameOrdinate(point: Point): Int = if (y == point.y) 0 else if (y > point.y) 1 else -1
  }
}


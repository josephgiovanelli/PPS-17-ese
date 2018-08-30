package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.view.Position
import Hippocampus.locationalFieldSize
import it.unibo.pps.ese.utils.Point
import Hippocampus.{worldWidth, worldHeight}

trait LocationalField {
  def topLeftPosition: Position
  def bottomRightPosition: Position
  def centerPosition: Position
  def distanceFromPosition(position: Position): Double
}

object LocationalField {

  def apply(centerPosition: Position): LocationalField = LocationalFieldImpl(centerPosition)

  private case class LocationalFieldImpl(var centerPosition: Position) extends LocationalField {

    var x: Double = centerPosition.x
    var y: Double = centerPosition.y

    if(x<locationalFieldSize) x=locationalFieldSize
    if(x>worldWidth-locationalFieldSize) x=worldWidth-locationalFieldSize
    if(y<locationalFieldSize) y=locationalFieldSize
    if(y>worldHeight-locationalFieldSize) y=worldHeight-locationalFieldSize

    centerPosition=Position(x,y)

    val topLeftPosition: Position = centerPosition-locationalFieldSize
    val bottomRightPosition: Position = centerPosition+locationalFieldSize

    private def containsPosition(position: Position): Boolean = {
      position >=&& topLeftPosition && position <=&& bottomRightPosition
    }

    override def distanceFromPosition(position: Position): Double = {
      if (containsPosition(position)) 0 else centerPosition |-| position
    }
  }

  implicit def positionToLocationalField(position: Position): LocationalField = {
    LocationalFieldImpl(position)
  }

  implicit def bound(tuple2: (Int, Int)): Point = {
    def bound(i: Int, bound: Int): Int = if (i < 0) 0 else if (i > bound) bound else i
    def boundWidth(x: Int): Int = bound(x, worldWidth)
    def boundHeight(y: Int): Int = bound(y, worldHeight)
    Point(boundWidth(tuple2._1), boundHeight(tuple2._2))
  }
}

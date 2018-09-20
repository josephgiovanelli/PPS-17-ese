package it.unibo.pps.ese.model.components.animals.brain.cerebralcortex

import it.unibo.pps.ese.utils.{Point, Position}

trait LocationalField {
  def topLeftPosition: Position
  def bottomRightPosition: Position
  def centerPosition: Position
  def distanceFromPosition(position: Position): Double
}

object LocationalField {

  def apply(worldWidth: Int, worldHeight: Int, locationalFieldSize: Double, centerPosition: Position): LocationalField =
    LocationalFieldImpl(worldWidth, worldHeight, locationalFieldSize, centerPosition)

  private case class LocationalFieldImpl(worldWidth: Int, worldHeight: Int, locationalFieldSize: Double, var centerPosition: Position) extends LocationalField {

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

}

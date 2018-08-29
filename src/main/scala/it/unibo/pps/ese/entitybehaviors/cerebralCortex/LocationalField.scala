package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.view.Position
import Hippocampus.locationalFieldSize

trait LocationalField {
  def distanceFromPosition(position: Position): Double
}

object LocationalField {

  def apply(topLeftPosition: Position, bottomRightPosition: Position): LocationalField = LocationalFieldImpl(topLeftPosition, bottomRightPosition)

  private case class LocationalFieldImpl(topLeftPosition: Position, bottomRightPosition: Position) extends LocationalField {

    val centerPosition = Position(topLeftPosition.x+locationalFieldSize, topLeftPosition.y+locationalFieldSize)

    def containsPosition(position: Position): Boolean = {
      position >= topLeftPosition && position <= bottomRightPosition
    }

    override def distanceFromPosition(position: Position): Double = ???
  }

  implicit def positionToLocationalField(position: Position): LocationalField = {
    LocationalFieldImpl(position-locationalFieldSize, position+locationalFieldSize)
  }
}

package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.view.Position

trait LocationalField

object LocationalField {

  def apply(upLeftPosition: Position, bottomRightPosition: Position): LocationalField = LocationalFieldImpl(upLeftPosition, bottomRightPosition)

  private case class LocationalFieldImpl(upLeftPosition: Position, bottomRightPosition: Position) extends LocationalField {
    def containsPosition(position: Position): Boolean = {
      position >= upLeftPosition && position <= bottomRightPosition
    }
  }

  implicit def positionToLocationalField(position: Position): LocationalField = {
    import Hippocampus.locationalFieldSize
    LocationalFieldImpl(position-locationalFieldSize, position+locationalFieldSize)
  }
}

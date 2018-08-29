package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.view.Position


object Location {
  case class LocationalField(startPosition: Position, endPosition: Position) {
    def containsPosition(position: Position): Boolean = {
      position >= startPosition && position <= endPosition
    }
  }
}

class Hippocampus {

}

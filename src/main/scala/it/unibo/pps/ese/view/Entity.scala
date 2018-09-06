package it.unibo.pps.ese.view

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Position
import scalafx.scene.paint.Color

case class Entity(id: String,
                  name: String,
                  position: Position,
                  color: Color)
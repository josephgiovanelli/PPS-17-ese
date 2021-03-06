package it.unibo.pps.ese.view.sections.configuration.visualization.core.components

import scalafx.scene.control.Label

/**
  * A white label
  *
  * @param text the text of the label
  */
class WhiteLabel(text: String, fontSize: Int = 18) extends Label(text) {
  style = "-fx-text-fill:white;"+
    s"-fx-font-size: ${fontSize}px;"
}

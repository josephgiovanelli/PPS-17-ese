package it.unibo.pps.ese.view.sections.configuration.visualization.core.components

import scalafx.scene.control.Label

class WhiteLabel(text: String, fontSize: Int = 18) extends Label(text) {
  style = "-fx-text-fill:white;"+
    s"-fx-font-size: ${fontSize}px;"
}

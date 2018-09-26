package it.unibo.pps.ese.view.sections.configuration.visualization.core.components

import scalafx.scene.control.Label

/**
  * A custom label for error message
  *
  * @param text the text of the label
  */
class ErrorLabel(text: String) extends Label {

  style = "-fx-text-fill: #E74636;"
}

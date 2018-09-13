package it.unibo.pps.ese.view.bodyViewer

import scalafx.scene.layout.HBox
import it.unibo.pps.ese.view.utilities.TextUtilities._
import scalafx.application.Platform
import scalafx.geometry.Pos
import scalafx.scene.text.TextFlow

sealed trait OrganViewer extends HBox{
  def setText(string: String)
  def clearText():Unit
}
case class OrganDescriptionBox() extends OrganViewer {
  val cssLayout:String =
    "-fx-padding: 5 5 5 5;\n"+
      "-fx-border-color: white;\n" +
      "-fx-border-insets: 5;\n" +
      "-fx-border-width: 3 0 3 0;\n" +
      "-fx-border-style: solid;\n"
  style = cssLayout
  minWidth = 250
  minHeight = 250
  maxWidth = 250
  maxHeight = 250
  alignment = Pos.CenterLeft
  private val textLabel = new TextFlow()
  textLabel.setMaxWidth(200)
  children.add(textLabel)

  override def setText(string: String): Unit = {
    Platform.runLater {
      () -> {
        textLabel.addAndClear(string.toTextOrgan)
      }
    }

  }

  override def clearText(): Unit = textLabel.children.clear()
}

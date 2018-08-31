package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs

import javafx.scene.Node

import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.stage.Window

case class EffectDialog(window: Window, currentEffect: Option[(String, Double)]) extends Dialog[(String, Double)] {

  initOwner(window)
  title = "Effect Dialog"
  headerText = "Define an allele effect"

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)


  val propertyName: TextField = new TextField() {
    promptText = "Name"
  }

  val effectValue: TextField = new TextField() {
    promptText = "Value"
  }

  val requiredField = Seq(propertyName, effectValue)

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Property"), 0, 0)
    add(propertyName, 1, 0)
    add(new Label("Value"), 0, 1)
    add(effectValue, 1, 1)
  }


  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(grid)
    styleClass += "sample-page"
  }

  if (currentEffect.isDefined) {
    propertyName.editable = false
    propertyName.text.value = currentEffect.get._1
    effectValue.text.value = currentEffect.get._2.toString
  }

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) (propertyName.text.value, effectValue.text.value.toDouble)
    else null
}





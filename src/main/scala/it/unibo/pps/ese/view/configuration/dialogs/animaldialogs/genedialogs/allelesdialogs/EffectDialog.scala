package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.ParseUtils

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class EffectDialog(window: Window, currentEffect: (String, Double)) extends Dialog[(String, Double)] {

  /*
  Header
   */

  initOwner(window)
  title = "Effect Dialog"
  headerText = "Define an allele effect"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  /*
  Fields
   */


  val propertyName: TextField = new TextField()
  val effectValue: TextField = new TextField()

  val fields: Map[TextField, (Label, Label)] = ListMap(
    propertyName -> (new Label("Name"), new Label("")),
    effectValue -> (new Label("Value"), new Label(""))
  )

  val grid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(10, 100, 10, 10)

    var count = 0
    fields.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
  }

  dialogPane().content = grid

  Platform.runLater(propertyName.requestFocus())

  /*
  OkButton
   */

  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false

  /*
  Checks
   */

  val mandatoryFields: Set[TextField] = fields.keySet
  val doubleFields: Set[TextField] = mandatoryFields - propertyName

  mandatoryFields.foreach(subject => {
    subject.text.onChange ( (_, _, newValue) => {
      okButton.disable = checkFields(subject, newValue)
    })
  })


  /*
  Restart information
   */

  propertyName.editable = false
  propertyName.text.value = currentEffect._1
  effectValue.text.value = currentEffect._2.toString

  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) (propertyName.text.value, effectValue.text.value.toDouble)
    else null


  private def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatoryCheck = field.getText.trim().isEmpty
    val doubleCheck = if (doubleFields.contains(field)) ParseUtils.parse[Double](field.getText.trim()).isEmpty else false

    if (mandatoryCheck || doubleCheck)
      field.pseudoClassStateChanged(errorClass, true)
    else
      field.pseudoClassStateChanged(errorClass, false)

    if (mandatoryCheck) fields(field)._2.text.value = "Must be filled"
    else if (doubleCheck) fields(field)._2.text.value = "Must be double"
    else fields(field)._2.text.value = ""

    checkFields
  }

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) || doubleFields.exists(x => ParseUtils.parse[Double](x.getText.trim()).isEmpty)

}





package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.ParseUtils

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class ConversionMapDialog(window: Window, currentConversion: Option[(String, Double)], qualites: Set[String]) extends Dialog[(String, Double)] {

  /*
  Header
  */

  initOwner(window)
  title = "Conversion Map Dialog"
  headerText = "Define a conversion map"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  /*
  Fields
   */

  val conversionName = new ComboBox(ObservableBuffer[String](qualites.toSeq))
  val previousConversionName = new TextField()

  val conversionValue: TextField = new TextField()

  val fields: Map[TextField, (Label, Label)] = ListMap(
    conversionValue -> (new Label("Value"), new Label("")),
  )

  val requiredField = Seq(conversionName, conversionValue)

  val grid: GridPane = new GridPane() {
    hgap = 20
    vgap = 20
    padding = Insets(20, 100, 10, 10)

    add(new Label("Name"), 0, 0)
    add(if (currentConversion.isDefined) previousConversionName else conversionName, 1, 0)
    var count = 1
    fields.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
  }

  dialogPane().content = new VBox() {
    children ++= Seq(grid)
    styleClass += "sample-page"
  }

  /*
  OkButton
   */

  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  /*
  Checks
   */

  val mandatoryFields: Set[TextField] = fields.keySet
  val doubleFields: Set[TextField] = fields.keySet

  mandatoryFields.foreach(subject =>
    subject.text.onChange ((_, _, newValue) =>
      okButton.disable = checkFields(subject, newValue)))

  /*
  Restart information
  */

  if (currentConversion.isDefined) {
    conversionName.value.value = currentConversion.get._1
    previousConversionName.editable = false
    previousConversionName.text.value = currentConversion.get._1
    conversionValue.text.value = currentConversion.get._2.toString
  } else {
    conversionName.value.value = qualites.head
  }

  /*
  Result
  */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) (conversionName.value.value, conversionValue.text.value.toDouble)
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

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) || doubleFields.exists(x => ParseUtils.parse[Int](x.getText.trim()).isEmpty)

}




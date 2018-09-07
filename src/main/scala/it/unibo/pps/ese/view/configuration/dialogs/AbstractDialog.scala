package it.unibo.pps.ese.view.configuration.dialogs

import scalafx.Includes._
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color
import scalafx.stage.Window

abstract class AbstractDialog[A](window: Window, key: Option[String] = None) extends Dialog[A] {

  initOwner(window)
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  var fields: Map[TextField, (Label, Label)] = Map.empty
  var mandatoryFields: Set[TextField] = Set.empty
  var intFields: Set[TextField] = Set.empty
  var doubleFields: Set[TextField] = Set.empty
  var uniqueFields: Map[TextField, Set[String]] = Map.empty

  /*
  OkButton
   */

  val okButtonType: ButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true


  def createGrid: GridPane =
    new GridPane() {
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


  def createChecks(): Unit =
    mandatoryFields.foreach(subject =>
      subject.text.onChange ((_, _, newValue) =>
        okButton.disable = checkFields(subject, newValue)))

  def mandatoryCheck(field: TextField): Boolean =
    field.getText.trim().isEmpty

  def intCheck(field: TextField): Boolean =
    if (intFields.contains(field)) ParseUtils.parse[Int](field.getText.trim()).isEmpty else false

  def doubleCheck(field: TextField): Boolean =
    if (doubleFields.contains(field)) ParseUtils.parse[Double](field.getText.trim()).isEmpty else false

  def uniqueCheck(field: TextField): Boolean =
    if (key.isEmpty && uniqueFields.keySet.contains(field)) uniqueFields(field).contains(field.text.value) else false


  def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatory = mandatoryCheck(field)
    val int = intCheck(field)
    val double = doubleCheck(field)
    val unique = uniqueCheck(field)

    if (mandatory || int || double || unique)
      field.pseudoClassStateChanged(errorClass, true)
    else
      field.pseudoClassStateChanged(errorClass, false)

    if (mandatory) fields(field)._2.text.value = "Must be filled"
    else if (int) fields(field)._2.text.value = "Must be int"
    else if (double) fields(field)._2.text.value = "Must be double"
    else if (unique) fields(field)._2.text.value = "Must be unique"
    else fields(field)._2.text.value = ""

    checkFields
  }

  def checkFields: Boolean = mandatoryFields.exists(field => mandatoryCheck(field)) ||
    intFields.exists(field => intCheck(field)) ||
    doubleFields.exists(field => doubleCheck(field)) ||
    uniqueFields.keySet.exists(field => uniqueCheck(field))

}

package it.unibo.pps.ese.view.configuration.dialogs

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color
import scalafx.stage.Window

abstract class AbstractDialog[A](window: Window, key: Option[String] = None) extends Dialog[A] {

  /*
  Dialog Setup
   */
  initOwner(window)
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  /*
  Fields
   */
  var fields: Map[TextField, (Label, Label)] = Map.empty
  var mandatoryFields: Set[TextField] = Set.empty
  var intFields: Set[TextField] = Set.empty
  var doubleFields: Set[TextField] = Set.empty
  var uniqueFields: Map[TextField, Set[String]] = Map.empty
  var listFields: Seq[ObservableBuffer[String]] = Seq.empty
  var lengthFields: Map[TextField, Int] = Map.empty
  var probabilityFields: Set[TextField] = Set.empty

  /*
  OkButton
   */

  val okButtonType: ButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true


  /*
  Methods
   */

  def createGrid(initRow: Int): GridPane =
    new GridPane() {
      hgap = 10
      padding = Insets(10, 100, 10, 10)

      var count: Int = initRow
      fields.foreach(field => {
        add(field._2._1, 0, count)
        add(field._1, 1, count)
        count += 1
        add(field._2._2, 1, count)
        count += 1
        field._2._2.textFill = Color.Red
      })
    }


  def createChecks(): Unit = {
    mandatoryFields.foreach(subject =>
      subject.text.onChange ((_, _, newValue) =>
        okButton.disable = checkFields(subject, newValue)))

    listFields.foreach(subject =>
      subject.onChange ((_, _) =>
        okButton.disable = checkFields))
  }


  def mandatoryCheck(field: TextField): Boolean =
    field.getText.trim().isEmpty

  def intCheck(field: TextField): Boolean =
    if (intFields.contains(field)) ParseUtils.parse[Int](field.getText.trim()).isEmpty else false

  def doubleCheck(field: TextField): Boolean =
    if (doubleFields.contains(field)) ParseUtils.parse[Double](field.getText.trim()).isEmpty else false

  def uniqueCheck(field: TextField): Boolean =
    if (key.isEmpty && uniqueFields.keySet.contains(field)) uniqueFields(field).contains(field.text.value) else false

  def lengthCheck(field: TextField): Boolean =
    if (lengthFields.keySet.contains(field)) field.text.value.length != lengthFields(field) else false

  def probabilityCheck(field:TextField): Boolean =
    if (probabilityFields.contains(field))
      if (ParseUtils.parse[Double](field.getText.trim()).isEmpty)
        true
      else
        field.text.value.toDouble < 0.0 || field.text.value.toDouble > 1.0
    else false

  def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatory = mandatoryCheck(field)
    val int = intCheck(field)
    val double = doubleCheck(field)
    val unique = uniqueCheck(field)
    val length = lengthCheck(field)
    val probability = probabilityCheck(field)

    if (mandatory || int || double || unique || length || probability)
      field.pseudoClassStateChanged(errorClass, true)
    else
      field.pseudoClassStateChanged(errorClass, false)

    if (mandatory) fields(field)._2.text.value = "Must be filled"
    else if (int) fields(field)._2.text.value = "Must be int"
    else if (double) fields(field)._2.text.value = "Must be double"
    else if (unique) fields(field)._2.text.value = "Must be unique"
    else if (length) fields(field)._2.text.value = "Must be " + lengthFields(field) + " long"
    else if (probability) fields(field)._2.text.value = "Must be a probability"
    else fields(field)._2.text.value = ""

    checkFields
  }

  def checkFields: Boolean = mandatoryFields.exists(field => mandatoryCheck(field)) ||
    intFields.exists(field => intCheck(field)) ||
    doubleFields.exists(field => doubleCheck(field)) ||
    uniqueFields.keySet.exists(field => uniqueCheck(field)) ||
    lengthFields.keySet.exists(field => lengthCheck(field)) ||
    probabilityFields.exists(field => probabilityCheck(field)) ||
    listFields.exists(x => x.isEmpty)

}

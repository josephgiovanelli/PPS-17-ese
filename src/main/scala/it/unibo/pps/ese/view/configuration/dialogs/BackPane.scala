package it.unibo.pps.ese.view.configuration.dialogs

import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{Button, ButtonType, Label, TextField}
import scalafx.scene.layout.{BorderPane, GridPane, Pane}
import scalafx.scene.paint.Color

trait Modality
case object AddModality extends Modality
case object ModifyModality extends Modality

abstract class DialogPane extends BorderPane {
  var title: String = ""
  var headerText: String = ""

}

abstract class BackPane[A](mainDialog: MainDialog, val previousContent: Option[DialogPane], val key: Option[String]) extends DialogPane {

  prefWidth = 500
  prefHeight = 600


  if (previousContent.isDefined) {
    val backButton = new Button("Back")

    backButton.onAction = _ => {
      mainDialog.setContent(previousContent.get)
    }

    top = backButton
  }



  object ParseUtils {
    case class ParseOp[T](op: String => T)

    implicit val popDouble: ParseOp[Double] = ParseOp[Double](_.toDouble)
    implicit val popInt: ParseOp[Int] = ParseOp[Int](_.toInt)

    def parse[T: ParseOp](s: String): Option[T] = try { Some(implicitly[ParseOp[T]].op(s)) }
    catch { case _ => None }
  }

  /*
  Dialog Setup
   */


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

  val okButton = new Button("Confirm")
  bottom = okButton

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

    okButton.disable = checkFields
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

  def checkFields: Boolean = {
    (mandatoryFields.exists(field => mandatoryCheck(field)) && mandatoryFields.nonEmpty) ||
      (intFields.exists(field => intCheck(field)) && intFields.nonEmpty) ||
      (doubleFields.exists(field => doubleCheck(field)) && doubleFields.nonEmpty) ||
      (uniqueFields.keySet.exists(field => uniqueCheck(field)) && uniqueFields.nonEmpty) ||
      (lengthFields.keySet.exists(field => lengthCheck(field)) && lengthFields.nonEmpty) ||
      (probabilityFields.exists(field => probabilityCheck(field)) && probabilityFields.nonEmpty) ||
      listFields.exists(x => x.isEmpty)
  }

}

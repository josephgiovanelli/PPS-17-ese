package it.unibo.pps.ese.view.configuration.dialogs.plantdialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.{EntitiesInfo, ParseUtils, PlantInfo}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.paint.Color
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.stage.Window

case class PlantDialog(window: Window, key: Option[String] = None) extends Dialog[String] {

  /*
  Header
   */

  initOwner(window)
  title = "Plant Dialog"
  headerText = "Create a plant"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  /*
  Fields
   */

  val name: TextField = new TextField()
  val heightPlant: TextField = new TextField()
  val nutritionalValue: TextField = new TextField()
  val hardness: TextField = new TextField()
  val availability: TextField = new TextField()

  val fields: Map[TextField, (Label, Label)] = ListMap(
    name -> (new Label("Name"), new Label("")),
    heightPlant -> (new Label("Height"), new Label("")),
    nutritionalValue -> (new Label("Nutritional Value"), new Label("")),
    hardness -> (new Label("Hardness"), new Label("")),
    availability -> (new Label("Availability"), new Label("")),
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

  Platform.runLater(name.requestFocus())

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
  val doubleFields: Set[TextField] = mandatoryFields - name
  val uniqueFields: Set[TextField] = Set(name)

  mandatoryFields.foreach(subject => {
    subject.text.onChange ( (_, _, newValue) => {
      okButton.disable = checkFields(subject, newValue)
    })
  })

  /*
  Restart information
   */

  if (key.isDefined) {
    val plantInfo = EntitiesInfo.instance().getPlantInfo(key.get) match {
      case Some(value) => value
      case None => throw new IllegalStateException()
    }
    name.editable = false
    name.text.value = key.get
    heightPlant.text.value = plantInfo.height.toString
    nutritionalValue.text.value = plantInfo.nutritionalValue.toString
    hardness.text.value = plantInfo.hardness.toString
    availability.text.value = plantInfo.availability.toString
  }

  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      EntitiesInfo.instance().setPlantInfo(name.text.value, PlantInfo(heightPlant.text.value.toDouble, nutritionalValue.text.value.toDouble, hardness.text.value.toDouble, availability.text.value.toDouble))
      name.text.value
    }
    else
      null


  private def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatoryCheck = field.getText.trim().isEmpty
    val doubleCheck = if (doubleFields.contains(field)) ParseUtils.parse[Double](field.getText.trim()).isEmpty else false
    val uniqueCheck = if (key.isEmpty && uniqueFields.contains(field)) EntitiesInfo.instance().getPlants.contains(field.text.value) || EntitiesInfo.instance().getAnimals.contains(field.text.value)
                      else false

    if (mandatoryCheck || doubleCheck || uniqueCheck)
      field.pseudoClassStateChanged(errorClass, true)
    else
      field.pseudoClassStateChanged(errorClass, false)

    if (mandatoryCheck) fields(field)._2.text.value = "Must be filled"
    else if (doubleCheck) fields(field)._2.text.value = "Must be double"
    else if (uniqueCheck) fields(field)._2.text.value = "Must be unique"
    else fields(field)._2.text.value = ""

    checkFields
  }

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) ||
    doubleFields.exists(x => ParseUtils.parse[Double](x.getText.trim()).isEmpty) ||
    (uniqueFields.exists(x => EntitiesInfo.instance().getPlants.contains(x.text.value)) && key.isEmpty)



}

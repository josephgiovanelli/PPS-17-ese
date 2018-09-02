package it.unibo.pps.ese.view.configuration.dialogs.plantdialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.{EntitiesInfo, ParseUtils, PlantInfo}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.beans.property.StringProperty
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

  /*
  Fields
   */

  val name: TextField = new TextField()
  val heightPlant: TextField = new TextField()
  val nutritionalValue: TextField = new TextField()
  val hardness: TextField = new TextField()
  val availability: TextField = new TextField()
  val errorLabel = new Label()

  val fields: Map[TextField, Label] = ListMap(
    name -> new Label("Name"),
    heightPlant -> new Label("Height"),
    nutritionalValue -> new Label("Nutritional Value"),
    hardness -> new Label("Hardness"),
    availability -> new Label("Availability"),
  )

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    var count = 0
    fields.foreach(field => {
      add(field._2, 0, count)
      add(field._1, 1, count)
      count += 1
    })
  }

  dialogPane().content = new VBox(grid, errorLabel)

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
  val doubleField: Set[TextField] = mandatoryFields - name
  val error: StringProperty = StringProperty(checkFields())
  errorLabel.text <== error

  mandatoryFields.foreach(subject => {
    subject.text.onChange ( (_, _, _) => {
      error.value = checkFields()
      okButton.disable = mandatoryFields.exists(x => x.getText.trim().isEmpty) ||
        doubleField.exists(x => ParseUtils.parse[Double](x.getText.trim()).isEmpty)
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


  private def checkFields(): String = {
    val mandatoryCheck = mandatoryFields.filter(x => x.getText.trim().isEmpty)
    val doubleCheck = doubleField.filter(x => ParseUtils.parse[Double](x.getText.trim()).isEmpty)
    var checksSuccessful = true
    var toPrint: String = ""
    if (mandatoryCheck.nonEmpty) {
      toPrint = "Empty fields: " + mandatoryCheck.map(field => fields(field).text.value).foldRight("")(_ + " | " + _) + "\n"
      checksSuccessful = false
    }
    if (doubleCheck.nonEmpty) {
      toPrint += "Double fields: " + doubleCheck.map(field => fields(field).text.value).foldRight("")(_ + " | " + _)
      checksSuccessful = false
    }
    toPrint
  }

}

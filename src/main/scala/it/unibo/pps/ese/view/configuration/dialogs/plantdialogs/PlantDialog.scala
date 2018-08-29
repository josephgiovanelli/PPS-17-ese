package it.unibo.pps.ese.view.configuration.dialogs.plantdialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.{EntitiesInfo, PlantInfo}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

case class PlantDialog(window: Window, key: Option[String] = None) extends Dialog[String] {
  initOwner(window)
  title = "Plant Dialog"
  headerText = "Create a plant"

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  // Create the username and password labels and fields.
  val name: TextField = new TextField() {
    promptText = "Name"
  }
  val heightPlant: TextField = new TextField() {
    promptText = "Height"
  }
  val nutritionalValue: TextField = new TextField() {
    promptText = "Nutritional Value"
  }
  val hardness: TextField = new TextField() {
    promptText = "Hardness"
  }
  val availability: TextField = new TextField() {
    promptText = "Availability"
  }

  val requiredField = Seq(name, heightPlant, nutritionalValue, hardness, availability)

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Name"), 0, 0)
    add(name, 1, 0)
    add(new Label("Height"), 0, 1)
    add(heightPlant, 1, 1)
    add(new Label("Nutritional Value"), 0, 2)
    add(nutritionalValue, 1, 2)
    add(new Label("Hardness"), 0, 3)
    add(hardness, 1, 3)
    add(new Label("Availability"), 0, 4)
    add(availability, 1, 4)
  }

  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  requiredField.foreach(subject => {
    subject.text.onChange { (_, _, newValue) =>
      okButton.disable = newValue.trim().isEmpty || requiredField.filter(x => !x.equals(subject)).exists(x => x.getText.trim().isEmpty)
    }
  })

  dialogPane().content = grid

  Platform.runLater(name.requestFocus())

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

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      EntitiesInfo.instance().setPlantInfo(name.text.value, PlantInfo(heightPlant.text.value.toDouble, nutritionalValue.text.value.toDouble, hardness.text.value.toDouble, availability.text.value.toDouble))
      name.text.value
    }
    else
      null

}

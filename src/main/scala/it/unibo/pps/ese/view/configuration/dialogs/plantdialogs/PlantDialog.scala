package it.unibo.pps.ese.view.configuration.dialogs.plantdialogs

import it.unibo.pps.ese.view.configuration.dialogs.{AbstractDialog, EntitiesInfo, PlantInfo}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.control._
import scalafx.stage.Window

case class PlantDialog(window: Window, key: Option[String] = None) extends AbstractDialog[String](window, key) {

  /*
  Header
   */

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

  fields = ListMap(
    name -> (new Label("Name"), new Label("")),
    heightPlant -> (new Label("Height"), new Label("")),
    nutritionalValue -> (new Label("Nutritional Value"), new Label("")),
    hardness -> (new Label("Hardness"), new Label("")),
    availability -> (new Label("Availability"), new Label("")))



  dialogPane().content = createGrid

  Platform.runLater(name.requestFocus())

  /*
  Checks
   */

  mandatoryFields = fields.keySet
  doubleFields = mandatoryFields - name
  uniqueFields = Map(name -> (EntitiesInfo.instance().getPlants ++ EntitiesInfo.instance().getAnimals))

  createChecks()

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

}

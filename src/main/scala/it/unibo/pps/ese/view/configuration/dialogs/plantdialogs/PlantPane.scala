package it.unibo.pps.ese.view.configuration.dialogs.plantdialogs

import it.unibo.pps.ese.view.configuration.dialogs.{AbstractDialog, BackPane, ConfigurationPane, MainDialog}
import it.unibo.pps.ese.view.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.plants.PlantInfo

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.stage.Window

case class PlantPane(mainDialog: MainDialog,
                     override val previousContent: Option[ConfigurationPane],
                     override val key: Option[String] = None) extends BackPane(mainDialog, previousContent, key) {

  /*
  Header
   */

  println(uniqueFields)

  mainDialog.title = "Plant Dialog"
  mainDialog.headerText = "Create a plant"

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



  center = createGrid(0)

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

  okButton.onAction = _ => {
    EntitiesInfo.instance().setPlantInfo(
      name.text.value,
      PlantInfo(heightPlant.text.value.toDouble,
      nutritionalValue.text.value.toDouble,
        hardness.text.value.toDouble,
        availability.text.value.toDouble))

    previousContent.get.addNewSpecies(name.text.value)
    mainDialog.setContent(previousContent.get)
  }



}

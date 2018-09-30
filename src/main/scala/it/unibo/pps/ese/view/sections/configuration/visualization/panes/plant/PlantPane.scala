package it.unibo.pps.ese.view.sections.configuration.visualization.panes.plant

import it.unibo.pps.ese.view.sections.configuration.visualization.panes._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.WhiteLabel
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.plants.PlantInfo
import it.unibo.pps.ese.view.sections.configuration.visualization.core.{AbstractPane, MainDialog, Modality}

import scala.collection.immutable.ListMap
import scalafx.application.Platform
import scalafx.scene.control._

/**
  * It defines the title and the header
  */
object PlantProperties {
  val title = "Plant Pane"
  val headerText = "Create a plant"
}

import PlantProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

/**
  * The pane that allows to insert a plant.
  *
  * @param mainDialog the main dialog with which communicating
  * @param previousContent the previous content
  * @param modality add or modify a plant
  * @param key the key of the entity
  */
case class PlantPane(mainDialog: MainDialog,
                     override val previousContent: Option[ConfigurationPane],
                     modality: Modality,
                     override val key: Option[String] = None)
  extends AbstractPane(mainDialog, previousContent, key, title, headerText, previousContent.get.path + newLine(1) + title, 1) {

  /*
  Fields
   */

  val name: TextField = new TextField()
  val heightPlant: TextField = new TextField()
  val nutritionalValue: TextField = new TextField()
  val hardness: TextField = new TextField()

  fields = ListMap(
    name -> (new WhiteLabel("Name"), new Label("")),
    heightPlant -> (new WhiteLabel("Height"), new Label("")),
    nutritionalValue -> (new WhiteLabel("Availability"), new Label("")),
    hardness -> (new WhiteLabel("Hardness"), new Label("")))


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
  }

  /*
  Result
   */

  okButton.onAction = _ => {
    EntitiesInfo.instance().setPlantInfo(
      name.text.value,
      PlantInfo(heightPlant.text.value.toDouble,
      nutritionalValue.text.value.toDouble,
        hardness.text.value.toDouble))

    previousContent.get.confirmPlantSpecies(modality, name.text.value)
  }



}

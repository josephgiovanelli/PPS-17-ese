package it.unibo.pps.ese.view.sections.configuration.visualization.panes

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.simulation.loader.data.CompletePlantData
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteSimulationBuildException
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{ErrorLabel, WhiteLabel}
import it.unibo.pps.ese.view.sections.configuration.visualization.core.{AbstractPane, DialogPane, MainDialog}
import it.unibo.pps.ese.view.core.{MainComponent, SetupViewBridge}
import it.unibo.pps.ese.view.start.{NoCompleteSimulationAlert, UnexpectedExceptionAlert}

import scala.util.{Failure, Success}
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.scene.paint.Color

object ConfirmProperties {
  val title = "Confirm Dialog"
  val headerText = "Choose number of entities for each species"
}

import it.unibo.pps.ese.view.sections.configuration.visualization.panes.ConfirmProperties._

case class ConfirmPane(mainDialog: MainDialog,
                       override val previousContent: Option[DialogPane],
                       setupViewBridge: Option[SetupViewBridge],
                       mainComponent: Option[MainComponent],
                       setUp: Boolean,
                       newAnimalSpecies: Seq[String] = Seq.empty,
                       newPlantSpecies: Seq[String] = Seq.empty,
                       previousAnimalsCount: Map[String, Int] = Map.empty,
                       previousPlantsCount: Map[String, Int] = Map.empty)
  extends AbstractPane[Unit](mainDialog, previousContent, None, title, headerText, title, 0) {

  /*
  Fields
   */

  val animalsEntities: Map[TextField, (WhiteLabel, ErrorLabel)] =
    EntitiesInfo.instance().getAnimals.map(x => new TextField() -> (new WhiteLabel(x), new ErrorLabel(""))).toMap
  val plantsEntities: Map[TextField, (WhiteLabel, ErrorLabel)] =
    EntitiesInfo.instance().getPlants.map(x => new TextField() -> (new WhiteLabel(x), new ErrorLabel(""))).toMap

  fields = animalsEntities ++ plantsEntities

  val animalsGrid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(10, 100, 10, 10)

    var count = 0
    animalsEntities.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
  }

  val animalsPane = new BorderPane()
  animalsPane.top = new WhiteLabel("Animals", 25)
  animalsPane.bottom = animalsGrid

  val plantsGrid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(10, 100, 10, 10)

    var count = 0
    plantsEntities.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
  }

  val plantsPane = new BorderPane()
  plantsPane.top = new WhiteLabel("Plants", 25)
  plantsPane.bottom = plantsGrid

  center = new VBox() {
    children ++= Seq(animalsPane, plantsPane)
    styleClass += "sample-page"
  }


  /*
  Checks
   */

  mandatoryFields = fields.keySet
  doubleFields = mandatoryFields

  createChecks()

  /*
  Restart information
   */

  previousAnimalsCount.foreach(animal => animalsEntities.filter(animalText => animalText._2._1.text.value.equals(animal._1))
                                                        .foreach(animalText => animalText._1.text.value = animal._2.toString))

  previousPlantsCount.foreach(plant => plantsEntities.filter(plantText => plantText._2._1.text.value.equals(plant._1))
                                                     .foreach(plantText => plantText._1.text.value = plant._2.toString))

  /*
  Result
   */

  okButton.onAction = _ => {
      val animals: Map[String, Int] = animalsEntities.map(animal => animal._2._1.text.value -> animal._1.text.value.toInt)
      val plants: Map[String, Int] = plantsEntities.map(plant => plant._2._1.text.value -> plant._1.text.value.toInt)
      EntitiesInfo.instance().getSimulationData(animals, plants) match {
        case Success(simulationData) =>
          if (setUp) {
            setupViewBridge.getOrElse(throw new IllegalStateException()).startSimulation(simulationData)
          } else {
            val newAnimals: Map[CompleteAnimalData, Int] = simulationData.animals.filter(animal => newAnimalSpecies.contains(animal._1.name))
            val newPlants: Map[CompletePlantData, Int] = simulationData.plants.filter(plant => newPlantSpecies.contains(plant._1.name))
            val oldAnimals: Map[String, Int] = animals.filter(animal => !newAnimalSpecies.contains(animal._1))
            val oldPlants: Map[String, Int] = plants.filter(plant => !newPlantSpecies.contains(plant._1))
            mainComponent.getOrElse(throw new IllegalStateException()).addEntities(oldAnimals, oldPlants, newAnimals, newPlants)
          }
        case Failure(exception: CompleteSimulationBuildException) =>
          NoCompleteSimulationAlert(mainDialog.window, exception).showAndWait()
        case Failure(exception) =>
          UnexpectedExceptionAlert(mainDialog.window, exception).showAndWait()
    }
    mainDialog.closeDialog()
  }

}

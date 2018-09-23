package it.unibo.pps.ese.view.sections.configuration.visualization.panes

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.simulation.loader.exception.ResourceAlreadyExistsException
import it.unibo.pps.ese.controller.simulation.loader.io.{Folder, IOResource}
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.AnimalPane
import it.unibo.pps.ese.view.sections.configuration.visualization.core._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{CustomListView, ErrorLabel, WhiteLabel}
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.plant.PlantPane
import it.unibo.pps.ese.view.core.{MainComponent, SetupViewBridge}
import it.unibo.pps.ese.view.start.ResourceExistsAlert.Buttons
import it.unibo.pps.ese.view.start.{ResourceExistsAlert, UnexpectedExceptionAlert}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.{Button, ListView}
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.FileChooser

object ConfigurationProperties {
  val title = "Configuration Pane"
  val headerText = "Insert or edit your species"
}

import it.unibo.pps.ese.view.sections.configuration.visualization.panes.ConfigurationProperties._

case class ConfigurationPane(mainDialog: MainDialog,
                             override val previousContent: Option[DialogPane],
                             setupViewBridge: Option[SetupViewBridge],
                             mainComponent: Option[MainComponent],
                             setUp: Boolean,
                             previousAnimalsCount: Map[String, Int] = Map.empty,
                             previousPlantsCount: Map[String, Int] = Map.empty)
                            (implicit executionContext: ExecutionContext)
  extends BackPane[Unit](mainDialog, previousContent, None, title, headerText, title) {

  val errorLabel = new ErrorLabel("")

  /*
  Fields
   */

  var newAnimalSpecies: Seq[String] = Seq.empty
  var newPlantSpecies: Seq[String] = Seq.empty


  val animalsName: ObservableBuffer[String] = ObservableBuffer[String](EntitiesInfo.instance().getAnimals toSeq)
  val animalsListView: ListView[String] = new CustomListView[String] {
    items = animalsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1 && ((!setUp && newAnimalSpecies.contains(value)) || setUp)) {
        mainDialog.setContent(AnimalPane(mainDialog, Some(ConfigurationPane.this), ModifyModality, Some(value)))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val plantsName: ObservableBuffer[String] = ObservableBuffer[String](EntitiesInfo.instance().getPlants toSeq)
  val plantsListView: ListView[String] = new CustomListView[String] {
    items = plantsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1 && ((!setUp && newPlantSpecies.contains(value)) || setUp)) {
        mainDialog.setContent(PlantPane(mainDialog, Some(ConfigurationPane.this), ModifyModality, Some(value)))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val animalsAddButton = new Button("Add")
  val plantsAddButton = new Button("Add")
  animalsAddButton.onAction = _ => mainDialog.setContent(AnimalPane(mainDialog, Some(this), AddModality))

  plantsAddButton.onAction = _ => mainDialog.setContent(PlantPane(mainDialog, Some(this), AddModality))


  val animalsPane = new BorderPane()
  animalsPane.left = new WhiteLabel("Animals")
  animalsPane.right = animalsAddButton
  animalsPane.margin = Insets(30,0,0,0)

  val plantsPane = new BorderPane()
  plantsPane.left = new WhiteLabel("Plants")
  plantsPane.right = plantsAddButton
  plantsPane.margin = Insets(30,0,0,0)

  /*
   * FILE SAVING
   */
  val fileChooser: FileChooser = new FileChooser() {
    title = "Save simulation YAML"
  }

  val saveButton: Button = new Button("Save") {
    onAction = _ => {
      val animalEntities: Map[String, Int] = animalsName.zip(List.fill(animalsName.size)(0)).toMap
      val plantEntities: Map[String, Int] = plantsName.zip(List.fill(plantsName.size)(0)).toMap
      val data: PartialSimulationData = EntitiesInfo.instance().getPartialSimulationData(animalEntities, plantEntities)
      val chosenFile: java.io.File = fileChooser.showSaveDialog(mainDialog.window)
      if (chosenFile != null) {
        this.disable = true
        IOResource(chosenFile.toURI.toURL).getParent() match {
          case Some(f: Folder) =>
            val saver = setupViewBridge.getOrElse(throw new IllegalStateException())
            handleSaveResult(saver.saveSimulationData(data, chosenFile.getName, f), saver, f)
          case _ =>
            throw new IllegalStateException()
        }
      }
    }
  }
  if (!setUp) {
    saveButton.disable = true
  }

  def handleSaveResult(saveResult: Future[Try[Unit]], saver: SetupViewBridge, target: Folder): Unit = saveResult onComplete  {
    case Success(result) =>
      result match {
        case Success(_) =>
          Platform.runLater(saveButton.disable = false)
        case Failure(exception: ResourceAlreadyExistsException) =>
          Platform.runLater(handleSaveFailure(exception, saver, target))
      }
    case Failure(exception) =>
      Platform.runLater(UnexpectedExceptionAlert(mainDialog.window, exception))
  }

  def handleSaveFailure(exception: ResourceAlreadyExistsException, saver: SetupViewBridge, target: Folder): Unit = {
    ResourceExistsAlert(mainDialog.window, exception.existingResource).showAndWait() match {
      case Some(Buttons.Override) =>
        handleSaveResult(saver.retrySave(target, Some(exception.existingResource)), saver, target)
      case Some(Buttons.OverrideAll) =>
        handleSaveResult(saver.retrySave(target, None, overrideAll = true), saver, target)
      case _ =>
    }
  }

  center = new VBox() {
    children ++= Seq(animalsPane, animalsListView, plantsPane, plantsListView, new WhiteLabel("At least one species per reign"), saveButton)
    styleClass += "sample-page"
  }

  saveButton.margin = Insets(40, 0, 0, 0)

  /*
  Checks
   */

  listFields = Seq((plantsName, 1))
  createChecks()

  /*
  Result
   */

  okButton.onAction = _ =>
      mainDialog.setContent(
        ConfirmPane(
          mainDialog,
          Some(ConfigurationPane.this),
          setupViewBridge,
          mainComponent,
          setUp,
          newAnimalSpecies,
          newPlantSpecies,
          previousAnimalsCount,
          previousPlantsCount))


  def confirmPlantSpecies(m: Modality, name: String): Unit = {
    m match {
      case AddModality =>
        if (!setUp) newPlantSpecies = newPlantSpecies :+ name.toString
        plantsName.insert(plantsName.size, name.toString)
      case ModifyModality =>
        Platform.runLater(plantsListView.selectionModel().clearSelection())
    }
    mainDialog.setContent(this)
  }

  def confirmAnimalSpecies(m: Modality, name: String): Unit = {
    m match {
      case AddModality =>
        if (!setUp) newAnimalSpecies = newAnimalSpecies :+ name.toString
        animalsName.insert(animalsName.size, name.toString)
      case ModifyModality =>
        Platform.runLater(animalsListView.selectionModel().clearSelection())
    }
    mainDialog.setContent(this)
  }


}

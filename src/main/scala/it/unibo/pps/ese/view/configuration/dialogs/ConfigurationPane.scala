package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.view.{MainComponent, SetupViewBridge}
import it.unibo.pps.ese.view.configuration.ConfigurationView
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.{AnimalPane, ChromosomePane}
import it.unibo.pps.ese.view.configuration.dialogs.plantdialogs.PlantPane
import it.unibo.pps.ese.view.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals.AnimalBaseInfo
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.Includes._
import scalafx.scene.control.{Button, Label, ListView}
import scalafx.scene.layout.{BorderPane, Pane, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Window
import it.unibo.pps.ese.controller.loader.data.AnimalData.PartialAnimalData
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.loader.exception.ResourceAlreadyExistsException
import it.unibo.pps.ese.controller.util.io.{File, Folder, IOResource}
import it.unibo.pps.ese.view.{MainComponent, SetupViewBridge}
import it.unibo.pps.ese.view.start.{ResourceExistsAlert, UnexpectedExceptionAlert}
import it.unibo.pps.ese.view.start.ResourceExistsAlert.Buttons
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.{FileChooser, Window}

import scala.util.{Failure, Success, Try}

case class ConfigurationPane(mainDialog: MainDialog,
                             override val previousContent: Option[DialogPane],
                             setupViewBridge: Option[SetupViewBridge],
                             mainComponent: Option[MainComponent],
                             setUp: Boolean,
                             previousAnimalsCount: Map[String, Int] = Map.empty,
                             previousPlantsCount: Map[String, Int] = Map.empty) extends BackPane[Unit](mainDialog, previousContent, None) {

  /*
  Header
   */

  title = "Configuration Dialog"
  headerText = if (setUp) "Insert or edit your species" else "Insert another species"

  val errorLabel = new Label("")
  errorLabel.textFill = Color.Red

  /*
  Fields
   */

  var newAnimalSpecies: Seq[String] = Seq.empty
  var newPlantSpecies: Seq[String] = Seq.empty


  val animalsName: ObservableBuffer[String] = ObservableBuffer[String](EntitiesInfo.instance().getAnimals toSeq)
  val animalsListView: ListView[String] = new ListView[String] {
    items = animalsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1 && ((!setUp && newAnimalSpecies.contains(value)) || setUp)) {
        mainDialog.setContent(AnimalPane(mainDialog, Some(ConfigurationPane.this), ModifyModality, Some(value)))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val plantsName: ObservableBuffer[String] = ObservableBuffer[String](EntitiesInfo.instance().getPlants toSeq)
  val plantsListView: ListView[String] = new ListView[String] {
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
  animalsPane.left = new Label("Animals")
  animalsPane.right = animalsAddButton

  val plantsPane = new BorderPane()
  plantsPane.left = new Label("Plants")
  plantsPane.right = plantsAddButton

  /*
   * FILE SAVING
   */
  val fileChooser = new FileChooser() {
    title = "Save simulation YAML"
  }

  val saveButton: Button = new Button("Save") {
    onAction = _ => {
      val animalEntities: Map[String, Int] = animalsName.zip(List.fill(animalsName.size)(0)).toMap
      val plantEntities: Map[String, Int] = plantsName.zip(List.fill(plantsName.size)(0)).toMap
      val data: PartialSimulationData = EntitiesInfo.instance().getPartialSimulationData(animalEntities, plantEntities)
      val chosenFile: java.io.File = fileChooser.showSaveDialog(mainDialog.window)
      if (chosenFile != null) {
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

  def handleSaveResult(saveResult: Try[Unit], saver: SetupViewBridge, target: Folder) = saveResult match {
    case Success(_) =>
    case Failure(exception: ResourceAlreadyExistsException) =>
      handleSaveFailure(exception, saver, target)
    case Failure(exception) =>
      UnexpectedExceptionAlert(mainDialog.window, exception)
  }

  def handleSaveFailure(exception: ResourceAlreadyExistsException, saver: SetupViewBridge, target: Folder): Unit = {
    ResourceExistsAlert(mainDialog.window, exception.existingResource).showAndWait() match {
      case Some(Buttons.Override) =>
        handleSaveResult(saver.retrySave(target, Some(exception.existingResource)), saver, target)
      case Some(Buttons.OverrideAll) =>
        handleSaveResult(saver.retrySave(target, None, true), saver, target)
      case _ =>
    }
  }

  center = new VBox() {
    children ++= Seq(animalsPane, animalsListView, plantsPane, plantsListView, new Label("At least one species per reign"), saveButton)
    styleClass += "sample-page"
  }

  /*
  Checks
   */

  listFields = Seq(plantsName)
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
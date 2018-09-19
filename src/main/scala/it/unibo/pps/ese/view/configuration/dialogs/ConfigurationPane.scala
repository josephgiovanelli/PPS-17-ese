package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.view.MainComponent
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

case class ConfigurationPane(mainDialog: MainDialog,
                             override val previousContent: Option[Pane],
                             mainComponent: MainComponent,
                             setUp: Boolean,
                             previousAnimalsCount: Map[String, Int] = Map.empty,
                             previousPlantsCount: Map[String, Int] = Map.empty) extends BackPane[Unit](mainDialog, previousContent, None) {

  /*
  Header
   */

  mainDialog.title = "Configuration Dialog"
  mainDialog.headerText = if (setUp) "Insert or edit your species" else "Insert another species"

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


  center = new VBox() {
    children ++= Seq(animalsPane, animalsListView, plantsPane, plantsListView, new Label("At least one species per reign"))
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

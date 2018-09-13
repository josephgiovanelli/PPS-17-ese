package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.view.MainComponent
import it.unibo.pps.ese.view.configuration.ConfigurationView
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.{AnimalDialog, ChromosomeDialog}
import it.unibo.pps.ese.view.configuration.dialogs.plantdialogs.PlantDialog
import it.unibo.pps.ese.view.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals.AnimalBaseInfo

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.Includes._
import scalafx.scene.control.{Button, Label, ListView}
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class ConfigurationDialog(window: Window,
                               mainComponent: MainComponent,
                               setUp: Boolean,
                               previousAnimalsCount: Map[String, Int] = Map.empty,
                               previousPlantsCount: Map[String, Int] = Map.empty) extends AbstractDialog[Unit](window, None) {

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
        AnimalDialog(window, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val plantsName: ObservableBuffer[String] = ObservableBuffer[String](EntitiesInfo.instance().getPlants toSeq)
  val plantsListView: ListView[String] = new ListView[String] {
    items = plantsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1 && ((!setUp && newPlantSpecies.contains(value)) || setUp)) {
        PlantDialog(window, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  animalsListView.prefHeight = MIN_ELEM * ROW_HEIGHT
  plantsListView.prefHeight <== MIN_ELEM * ROW_HEIGHT

  val animalsAddButton = new Button("Add")
  val plantsAddButton = new Button("Add")
  animalsAddButton.onAction = _ => AnimalDialog(window).showAndWait() match {
    case Some(name) =>
      if (!setUp) newAnimalSpecies = newAnimalSpecies :+ name.toString
      animalsName.insert(animalsName.size, name.toString)
    case None => println("Dialog returned: None")
  }
  plantsAddButton.onAction = _ => PlantDialog(window).showAndWait() match {
    case Some(name) =>
      if (!setUp) newPlantSpecies = newPlantSpecies :+ name.toString
      plantsName.insert(plantsName.size, name.toString)
    case None => println("Dialog returned: None")
  }

  val animalsPane = new BorderPane()
  animalsPane.left = new Label("Animals")
  animalsPane.right = animalsAddButton

  val plantsPane = new BorderPane()
  plantsPane.left = new Label("Plants")
  plantsPane.right = plantsAddButton


  dialogPane().content = new VBox() {
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

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      ConfirmDialog(window,
        mainComponent,
        setUp,
        newAnimalSpecies,
        newPlantSpecies,
        previousAnimalsCount,
        previousPlantsCount).showAndWait()
    }
    else
      null

}

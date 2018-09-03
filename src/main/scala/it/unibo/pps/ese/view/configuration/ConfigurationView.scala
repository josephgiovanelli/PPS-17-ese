package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.genetics.entities.QualityType
import it.unibo.pps.ese.view.configuration.dialogs.{ConfirmDialog, ListViewUtils}
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.AnimalDialog
import it.unibo.pps.ese.view.configuration.dialogs.plantdialogs.PlantDialog
import it.unibo.pps.ese.view.MainComponent

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}

trait ConfigurationView {
}

class ConfigurationViewImpl(mainComponent: MainComponent) extends Scene(250, 350) with ConfigurationView {


  val currentWindow: scalafx.stage.Window = this.window()

  val animalsName: ObservableBuffer[String] = ObservableBuffer[String]()
  val animalsListView: ListView[String] = new ListView[String] {
    items = animalsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
        if (selectionModel().getSelectedIndex != -1) {
          AnimalDialog(currentWindow, Some(value)).showAndWait()
          Platform.runLater(selectionModel().clearSelection())
        }
      })
  }

  val plantsName: ObservableBuffer[String] = ObservableBuffer[String]()
  val plantsListView: ListView[String] = new ListView[String] {
    items = plantsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        PlantDialog(currentWindow, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  animalsListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT
  plantsListView.prefHeight <== ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT

  val animalsAddButton = new Button("Add")
  val plantsAddButton = new Button("Add")
  animalsAddButton.onAction = _ => AnimalDialog(currentWindow).showAndWait() match {
    case Some(name) =>
      animalsName.insert(animalsName.size, name.toString)
    case None => println("Dialog returned: None")
  }
  plantsAddButton.onAction = _ => PlantDialog(currentWindow).showAndWait() match {
    case Some(name) =>
      plantsName.insert(plantsName.size, name.toString)
    case None => println("Dialog returned: None")
  }

  val animalsPane = new BorderPane()
  animalsPane.left = new Label("Animals")
  animalsPane.right = animalsAddButton

  val plantsPane = new BorderPane()
  plantsPane.left = new Label("Plants")
  plantsPane.right = plantsAddButton

  val confirmButton = new Button("Confirm")
  confirmButton.onAction = _ => ConfirmDialog(currentWindow, mainComponent).showAndWait()


  content = new VBox() {
    children ++= Seq(animalsPane, animalsListView, plantsPane, plantsListView, confirmButton)
    styleClass += "sample-page"
  }
}

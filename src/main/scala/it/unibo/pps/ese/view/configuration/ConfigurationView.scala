package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.view.configuration.dialogs.{ConfirmDialog, ListViewUtils}
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.AnimalDialog
import it.unibo.pps.ese.view.configuration.dialogs.plantdialogs.PlantDialog
import it.unibo.pps.ese.view.MainComponent

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.paint.Color

trait ConfigurationView {
}

class ConfigurationViewImpl(mainComponent: MainComponent) extends Scene(250, 350) with ConfigurationView {


  this.getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  val currentWindow: scalafx.stage.Window = this.window()

  val errorLabel = new Label("")
  errorLabel.textFill = Color.Red

  val confirmButton = new Button("Confirm")
  confirmButton.onAction = _ => {
    if (plantsName.isEmpty || animalsName.isEmpty) {
      confirmButton.pseudoClassStateChanged(errorClass, true)
      if (plantsName.isEmpty && animalsName.isEmpty) {
        errorLabel.text.value = "Plants and Animals "
      } else if (plantsName.isEmpty) {
        errorLabel.text.value = "Plants "
      } else {
        errorLabel.text.value = "Animals "
      }
      errorLabel.text.value += "missed"
    } else {
      confirmButton.pseudoClassStateChanged(errorClass, false)
      ConfirmDialog(currentWindow, mainComponent).showAndWait()
    }
  }

  val buttonPane = new BorderPane()
  buttonPane.left = errorLabel
  buttonPane.right = confirmButton

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
      if (plantsName.nonEmpty) {
        confirmButton.pseudoClassStateChanged(errorClass, false)
        errorLabel.text.value = ""
      } else if (!errorLabel.text.value.isEmpty) {
        errorLabel.text.value = "Plants missed"
      }
    case None => println("Dialog returned: None")
  }
  plantsAddButton.onAction = _ => PlantDialog(currentWindow).showAndWait() match {
    case Some(name) =>
      plantsName.insert(plantsName.size, name.toString)
      if (animalsName.nonEmpty) {
        confirmButton.pseudoClassStateChanged(errorClass, false)
        errorLabel.text.value = ""
      } else if (!errorLabel.text.value.isEmpty) {
        errorLabel.text.value = "Animals missed"
      }
    case None => println("Dialog returned: None")
  }

  val animalsPane = new BorderPane()
  animalsPane.left = new Label("Animals")
  animalsPane.right = animalsAddButton

  val plantsPane = new BorderPane()
  plantsPane.left = new Label("Plants")
  plantsPane.right = plantsAddButton


  content = new VBox() {
    children ++= Seq(animalsPane, animalsListView, plantsPane, plantsListView, buttonPane)
    styleClass += "sample-page"
  }
}

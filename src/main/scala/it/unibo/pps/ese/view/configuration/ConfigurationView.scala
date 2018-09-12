package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.view.configuration.dialogs.{ConfigurationDialog, ConfirmDialog}
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.AnimalDialog
import it.unibo.pps.ese.view.configuration.dialogs.plantdialogs.PlantDialog
import it.unibo.pps.ese.view.MainComponent
import it.unibo.pps.ese.view.configuration.entitiesinfo.EntitiesInfo

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


  val currentWindow: scalafx.stage.Window = this.window()

  val confirmButton = new Button("Confirm")
  confirmButton.onAction = _ => {
     ConfigurationDialog(this.window(), mainComponent, setUp = true).showAndWait()
  }

  content = new VBox() {
    children ++= Seq(confirmButton)
    styleClass += "sample-page"
  }
}

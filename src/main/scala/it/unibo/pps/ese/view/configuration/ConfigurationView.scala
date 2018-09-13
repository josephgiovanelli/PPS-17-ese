package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.view.configuration.dialogs.ConfigurationDialog
import it.unibo.pps.ese.view.MainComponent

import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control._

trait ConfigurationView {
}

class ConfigurationViewImpl(mainComponent: MainComponent) extends Scene(250, 350) with ConfigurationView {


  val currentWindow: scalafx.stage.Window = this.window()

  val confirmButton = new Button("Confirm")
  confirmButton.onAction = _ => {
     ConfigurationDialog(currentWindow, mainComponent, setUp = true).showAndWait()
  }

  content = confirmButton
}

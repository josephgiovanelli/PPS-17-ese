package it.unibo.pps.ese.view.start

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.loader.exception.PartialSimulationDataException
import it.unibo.pps.ese.controller.util.io.{File, IOResource}
import it.unibo.pps.ese.genericworld.controller.Controller
import it.unibo.pps.ese.view.{MainComponent, StartViewBridge, ViewLauncher}
import it.unibo.pps.ese.view.configuration.dialogs.ConfigurationDialog
import it.unibo.pps.ese.view.configuration.entitiesinfo.EntitiesInfo
import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, Priority, VBox}
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter

import scala.util.{Failure, Success, Try}

trait StartMenuView extends Scene {
}

object StartMenuView {

  def apply(viewController: StartViewBridge): StartMenuView = new StartMenuViewImpl(viewController)

  //TODO in IO package object? seems insecure
  private implicit def javaFileToMyFile(file: java.io.File): File = IOResource(file.toURI.toURL) match {
    case f: File =>
      f
    case _ =>
      throw new IllegalArgumentException
  }

  private class StartMenuViewImpl(viewController: StartViewBridge) extends Scene(250, 350) with StartMenuView {


    val currentWindow: scalafx.stage.Window = this.window()

    val fileChooser = new FileChooser() {
      title = "Open Simulation Config File"
      extensionFilters ++= Seq(new ExtensionFilter("Simulation Files", File.FileFormats.YAML.extensions.map("*" + _)) )
    }
    val loadButton = new Button("Load And Run Existing Simulation") {
      hgrow = Priority.Always
      vgrow = Priority.Always
      maxHeight = Double.MaxValue
      maxWidth = Double.MaxValue
      onAction = _ => {
        val file: java.io.File = fileChooser.showOpenDialog(currentWindow)
        if(file != null) {
          viewController.startSimulation(file, currentWindow) match {
            case Success(_) =>
            case Failure(exception) =>
              //TODO show exception pane(IO)
          }
        }
      }
    }
    val loadEditButton = new Button("Load And Edit Existing Simulation") {
      hgrow = Priority.Always
      vgrow = Priority.Always
      maxHeight = Double.MaxValue
      maxWidth = Double.MaxValue
      onAction = _ => {
        val file: java.io.File = fileChooser.showOpenDialog(currentWindow)
        if(file != null) {
          viewController.loadSimulation(file, currentWindow) match {
            case Success(_) =>
            case Failure(exception) =>
              //TODO show exception pane(IO)
          }
        }
      }
    }
    val createButton = new Button("Create New Simulation") {
      hgrow = Priority.Always
      vgrow = Priority.Always
      maxHeight = Double.MaxValue
      maxWidth = Double.MaxValue

      onAction = _ => viewController.launchSetup(currentWindow)
    }

    val vbox: VBox = new VBox() {
      fillWidth = true
      children = Seq(loadButton, loadEditButton, createButton)
      maxHeight = Double.MaxValue
      maxWidth = Double.MaxValue
    }

    val border = new BorderPane() {
      center = vbox
    }

    root = border
  }
}

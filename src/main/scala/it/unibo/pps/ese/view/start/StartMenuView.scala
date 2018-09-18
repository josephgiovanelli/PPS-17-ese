package it.unibo.pps.ese.view.start

import it.unibo.pps.ese.controller.util.io.{File, IOResource}
import it.unibo.pps.ese.view.StartViewBridge
import scalafx.Includes._
import scalafx.scene.Scene
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

  private class StartMenuViewImpl(startViewBridge: StartViewBridge) extends Scene(250, 350) with StartMenuView {


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
          startViewBridge.startSimulation(file, currentWindow) match {
            case Success(_) =>
            case Failure(exception) =>
              UnexpectedExceptionAlert(currentWindow, exception)
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
          startViewBridge.loadSimulation(file, currentWindow) match {
            case Success(_) =>
            case Failure(exception) =>
              UnexpectedExceptionAlert(currentWindow, exception)
          }
        }
      }
    }
    val createButton = new Button("Create New Simulation") {
      hgrow = Priority.Always
      vgrow = Priority.Always
      maxHeight = Double.MaxValue
      maxWidth = Double.MaxValue

      onAction = _ => startViewBridge.launchSetup(currentWindow)
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

package it.unibo.pps.ese.view.start

import it.unibo.pps.ese.controller.util.io.{File, IOResource}
import it.unibo.pps.ese.view.StartViewBridge
import javafx.scene.paint.ImagePattern
import javafx.scene.text.Font
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.image.Image
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.text.Text
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

  private class StartMenuViewImpl(startViewBridge: StartViewBridge) extends Scene(433, 650) with StartMenuView {

    val currentWindow: scalafx.stage.Window = this.window()
    val buttonStyle =
      "-fx-font-weight: 600;\n" +
      "-fx-font-family: 'Helvetica', Arial, sans-serif;\n" +
      "-fx-font-size: 11pt ;"
    val buttonBackground = new Background(Array(new BackgroundFill(Color.rgb(236, 240, 241,0.8), CornerRadii.Empty, Insets.Empty)))
    val fileChooser = new FileChooser() {
      title = "Open Simulation Config File"
      extensionFilters ++= Seq(new ExtensionFilter("Simulation Files", File.FileFormats.YAML.extensions.map("*" + _)) )
    }
    val vbox: VBox = new VBox() {
      fillWidth = true
      prefWidth <== width
      prefHeight <== height
      spacing = 80
    }
    val loadButton = new Button("Load And Run Existing Simulation") {
      background = buttonBackground
      textFill = Color.web("34495e")
      prefHeight = 40
      style = buttonStyle
//      font = Font.font("Calibri",19)
      margin = Insets(80,85,0,85)
      prefWidth <== vbox.width*0.6
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
      background = buttonBackground
      textFill = Color.web("34495e")
      prefHeight = 40
      style = buttonStyle
      margin = Insets(0,85,0,85)
      prefWidth <== vbox.width*0.6
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
      background = buttonBackground
      textFill = Color.web("34495e")
      prefHeight = 40
      style = buttonStyle
      margin = Insets(0,85,0,85)
      prefWidth <== vbox.width*0.6
      onAction = _ => startViewBridge.launchSetup(currentWindow)
    }

    vbox.children = List(loadButton, loadEditButton, createButton)
    val text = new Text{
      text = "Evolution Simulation Engine"
      margin  = Insets(30,55,0,55)
      fill = Color.White
    }
    text.prefWidth(width.value)
    text.style = "-fx-font-weight: 600;\n" +
      "-fx-font-family: 'Helvetica', Arial, sans-serif;\n" +
      "-fx-font-size: 20pt ;"
    val border = new BorderPane() {
      top  = text
      center = vbox
    }
    val back:Image = new Image("it.unibo.pps.ese.view/backgrounds/launcherBack.jpg")
    border.background = new Background(Array(new BackgroundFill(new ImagePattern(back),CornerRadii.Empty, Insets.Empty)))

    root = border
  }
}

package it.unibo.pps.ese.view.start

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.loader.exception.PartialSimulationDataException
import it.unibo.pps.ese.controller.util.io.{File, IOResource}
import it.unibo.pps.ese.view.MainComponent
import it.unibo.pps.ese.view.configuration.entitiesinfo.EntitiesInfo
import javafx.collections.ObservableList
import scalafx.Includes._
import scalafx.geometry.Insets
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

  def apply(mainComponent: MainComponent): StartMenuView = new StartMenuViewImpl(mainComponent)

  //TODO in IO package object? seems insecure
  private implicit def javaFileToMyFile(file: java.io.File): File = IOResource(file.toURI.toURL) match {
    case f: File =>
      f
    case _ =>
      throw new IllegalArgumentException
  }

  private class StartMenuViewImpl(mainComponent: MainComponent) extends Scene(250, 350) with StartMenuView {


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
          mainComponent.startSimulation(file) match {
            case Success(_) =>
              //TODO launch simulation view
            case Failure(exception: PartialSimulationDataException) =>
              EntitiesInfo.instance().loadSimulationData(exception.partialSimulationData.getAnimals.getOrElse(Iterable()).map(_._1),
                exception.partialSimulationData.getPlants.getOrElse(Iterable()).map(_._1))
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
          //val t: Try[PartialSimulationData] = mainComponent.loadSimulation(file)
          val t: Try[PartialSimulationData] = Success(YamlLoader.loadSimulation(file))
           t match {
            case Success(data) =>
              EntitiesInfo.instance().loadSimulationData(data.getAnimals.getOrElse(Iterable()).map(_._1),
                data.getPlants.getOrElse(Iterable()).map(_._1))
            case Failure(exception) =>
              new Alert(AlertType.Information, exception.toString).showAndWait()
           }
        }
      }
    }
    val createButton = new Button("Create New Simulation") {
      hgrow = Priority.Always
      vgrow = Priority.Always
      maxHeight = Double.MaxValue
      maxWidth = Double.MaxValue
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

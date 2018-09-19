package it.unibo.pps.ese.view

import it.unibo.pps.ese.controller.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.loader.exception.CompleteSimulationBuildException
import it.unibo.pps.ese.controller.util.io.{ExistingResource, File, Folder}
import it.unibo.pps.ese.genericworld.controller.{Controller, SimulationController}
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.configuration.dialogs.ConfigurationDialog
import it.unibo.pps.ese.view.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.start.{NoCompleteSimulationAlert, StartMenuView}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.stage.Window

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}


trait ViewLauncher

trait StartViewBridge {
  def startSimulation(file: File, currentWindow: Window): Try[Unit]
  def loadSimulation(file: File, currentWindow: Window): Try[Unit]
  def launchSetup(currentWindow: Window): Unit
}

trait SetupViewBridge {
  def startSimulation(data: CompleteSimulationData): Try[Unit]
  def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Try[Unit]
  def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Try[Unit]
}

object ViewLauncher {

  def apply(geneticsSimulator: GeneticsSimulator, controller: Controller)
           (implicit executionContext: ExecutionContext): ViewLauncher = new ViewLauncherImpl(geneticsSimulator, controller)

  private class ViewLauncherImpl(geneticsSimulator: GeneticsSimulator, controller: Controller)
                                (implicit executionContext: ExecutionContext)extends PrimaryStage with ViewLauncher
    with SetupViewBridge with StartViewBridge {
    title = "Evolution Simulation Engine"
    val simulationController: Option[SimulationController] = None
    this.scene = StartMenuView(this)

    def launchSetup(currentWindow: Window): Unit = {
      ConfigurationDialog(currentWindow, Option(this), None, setUp = true).showAndWait()
    }

    def startSimulation(file: File, currentWindow: Window): Try[Unit] = {
      controller.startSimulation(file) match {
        case Success(value) =>
          val mainView = View(geneticsSimulator, value)
          value.attachView(mainView, 30)
          this.hide()
          Success()
        case Failure(exception: CompleteSimulationBuildException) =>
          NoCompleteSimulationAlert(currentWindow, exception.buildException).showAndWait()
          EntitiesInfo.instance().loadSimulationData(exception.partialSimulationData.getAnimals.getOrElse(Iterable()).map(_._1),
            exception.partialSimulationData.getPlants.getOrElse(Iterable()).map(_._1))
          ConfigurationDialog(currentWindow, Option(this), None, setUp = true).showAndWait()
          Success()
        case Failure(exception) =>
          Failure(exception)
      }
    }
    //Editing simulazione
    def loadSimulation(file: File, currentWindow: Window): Try[Unit] = {
      controller.loadSimulation(file) match {
        case Success(data) =>
          EntitiesInfo.instance().loadSimulationData(data.getAnimals.getOrElse(Iterable()).map(_._1),
            data.getPlants.getOrElse(Iterable()).map(_._1))
          ConfigurationDialog(currentWindow, Option(this), None, setUp = true).showAndWait()
          Success()
        case Failure(exception) =>
          Failure(exception)
      }
    }

    def startSimulation(data: CompleteSimulationData): Try[Unit] = {
      controller.startSimulation(data) match {
        case Success(value) =>
          val mainView = View(geneticsSimulator, value)
          value.attachView(mainView, 30)
          this.hide()
          Success()
        case _ =>
          throw new IllegalStateException()
      }
    }

    //Cachare saver e target
    def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Try[Unit] = {
      Success()
    }
    def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Try[Unit] = {
      Success()
    }
  }
}

object ViewType extends Enumeration {
  type ViewType = Value
  val MainView, ConfigurationView, StartView = Value
}
package it.unibo.pps.ese.view.core

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteSimulationBuildException
import it.unibo.pps.ese.controller.simulation.loader.io.{ExistingResource, File, Folder}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators.{Controller, SimulationController}
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.sections.configuration.visualization.core.{ConfigurationContent, MainDialog}
import it.unibo.pps.ese.view.start.{NoCompleteSimulationAlert, StartMenuView}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.Platform
import scalafx.stage.Window


trait ViewLauncher

trait StartViewBridge {
  def startSimulation(file: File, currentWindow: Window): Future[Try[Unit]]
  def loadSimulation(file: File, currentWindow: Window): Future[Try[Unit]]
  def launchSetup(currentWindow: Window, simulationData: Option[PartialSimulationData] = None): Unit
}

trait SetupViewBridge {
  def startSimulation(data: CompleteSimulationData): Future[Try[Unit]]
  def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Future[Try[Unit]]
  def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Future[Try[Unit]]
}

object ViewLauncher {

  def apply(geneticsSimulator: GeneticsSimulator, controller: Controller)
           (implicit executionContext: ExecutionContext): ViewLauncher = new ViewLauncherImpl(geneticsSimulator, controller)

  private class ViewLauncherImpl(geneticsSimulator: GeneticsSimulator, controller: Controller)
                                (implicit executionContext: ExecutionContext)extends PrimaryStage with ViewLauncher
    with SetupViewBridge with StartViewBridge {
    title = "Evolution Simulation Engine"
    val simulationController: Option[SimulationController] = None
    val startMenuView = StartMenuView(this)
    this.scene = startMenuView

    def launchSetup(currentWindow: Window, simulationData: Option[PartialSimulationData] = None): Unit = {
      simulationData.foreach(populateEntitiesInfo)
      MainDialog(currentWindow, None, Option(this), setUp = true, ConfigurationContent).show()
      startMenuView.disableButtons(false)
    }

    def startSimulation(file: File, currentWindow: Window): Future[Try[Unit]] = {
      controller.startSimulation(file).map({
        case Success((contr, data)) =>
          populateEntitiesInfo(data)
          launchMainView(contr)
          Success(Unit)
        case Failure(exception: CompleteSimulationBuildException) =>
          Platform.runLater({
            NoCompleteSimulationAlert(currentWindow, exception).showAndWait()
            launchSetup(currentWindow, Some(exception.partialSimulationData))
          })
          Success(Unit)
        case Failure(exception) =>
          Failure(exception)
      })
    }

    def loadSimulation(file: File, currentWindow: Window): Future[Try[Unit]] = {
      controller.loadSimulation(file) map {
        case Success(data) =>
          Platform.runLater(launchSetup(currentWindow, Some(data)))
          Success(Unit)
        case Failure(exception) =>
          Failure(exception)
      }
    }

    def startSimulation(data: CompleteSimulationData): Future[Try[Unit]] = {
      controller.startSimulation(data) map {
        case Success(value) =>
          Platform.runLater({
            val mainView = View(geneticsSimulator, value)
            value.attachView(mainView, 30)
            this.hide()
          })
          Success(Unit)
        case Failure(exception) =>
          throw exception
      }
    }

    private def launchMainView(controller: SimulationController): Unit = {
      Platform.runLater({
        val mainView = View(geneticsSimulator, controller)
        controller.attachView(mainView, 30)
        this.hide()
      })
    }

    private def populateEntitiesInfo(data: PartialSimulationData): Unit = {
      EntitiesInfo.instance().loadSimulationData(data.getAnimals.getOrElse(Iterable()).map(_._1),
        data.getPlants.getOrElse(Iterable()).map(_._1))
    }

    def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Future[Try[Unit]] = {
      controller.saveSimulationData(simulation, simulationName, target)
    }
    def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Future[Try[Unit]] = {
      controller.retrySave(target, overrideResource, overrideAll)
    }
  }
}

object ViewType extends Enumeration {
  type ViewType = Value
  val MainView, ConfigurationView, StartView = Value
}
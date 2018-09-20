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
  def loadSimulation(file: File, currentWindow: Window): Try[Unit]
  def launchSetup(currentWindow: Window): Unit
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
    this.scene = StartMenuView(this)

    def launchSetup(currentWindow: Window): Unit = {
//      ConfigurationDialog(currentWindow, Option(this), None, setUp = true).showAndWait()
      MainDialog(currentWindow, None, Option(this), setUp = true, ConfigurationContent).show()
    }

    def startSimulation(file: File, currentWindow: Window): Future[Try[Unit]] = {
      controller.startSimulation(file).map({
        case Success((contr, data)) =>
          Platform.runLater({
            EntitiesInfo.instance().loadSimulationData(data.getAnimals.getOrElse(Iterable()).map(_._1),
              data.getPlants.getOrElse(Iterable()).map(_._1))
            val mainView = View(geneticsSimulator, contr)
            contr.attachView(mainView, 30)
            this.hide()
          })
          Success()
        case Failure(exception: CompleteSimulationBuildException) =>
          Platform.runLater({
            NoCompleteSimulationAlert(currentWindow, exception.buildException).showAndWait()
            EntitiesInfo.instance().loadSimulationData(exception.partialSimulationData.getAnimals.getOrElse(Iterable()).map(_._1),
              exception.partialSimulationData.getPlants.getOrElse(Iterable()).map(_._1))
            //          ConfigurationDialog(currentWindow, Option(this), None, setUp = true).showAndWait()
            MainDialog(currentWindow, None, Option(this), setUp = true, ConfigurationContent).show()
          })
          Success()
        case Failure(exception) =>
          Failure(exception)
      })
    }
    //Editing simulazione
    def loadSimulation(file: File, currentWindow: Window): Try[Unit] = {
      controller.loadSimulation(file) match {
        case Success(data) =>
          EntitiesInfo.instance().loadSimulationData(data.getAnimals.getOrElse(Iterable()).map(_._1),
            data.getPlants.getOrElse(Iterable()).map(_._1))
//          ConfigurationDialog(currentWindow, Option(this), None, setUp = true).showAndWait()
          MainDialog(currentWindow, None, Option(this), setUp = true, ConfigurationContent).show()
          Success()
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
          Success()
        case _ =>
          throw new IllegalStateException()
      }
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
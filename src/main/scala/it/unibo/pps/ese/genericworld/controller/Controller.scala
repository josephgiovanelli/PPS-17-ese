package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.controller.loader.{Saver, YamlLoader, YamlSaver}
import it.unibo.pps.ese.controller.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.util.io.{ExistingResource, File, Folder}
import it.unibo.pps.ese.genericworld.model.SimulationBuilder
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

trait Controller {
  //Partenza simulazione
  def startSimulation(file: File): Try[SimulationController]
  def startSimulation(data: CompleteSimulationData): Try[SimulationController]
  //Editing simulazione
  def loadSimulation(file: File): Try[PartialSimulationData]
  //Cachare saver e target
  def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Try[Unit]
  def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Try[Unit]
}
object Controller {

  def apply()(implicit executionContext: ExecutionContext): Controller = new BaseController()

  private class BaseController()(implicit executionContext: ExecutionContext) extends Controller {

    var saver: Option[Saver] = None

    override def startSimulation(data: CompleteSimulationData): Try[SimulationController] = {
      Success(new SimulationBuilder[EmptySimulation]
        .dimension(500, 500)
        .data(data)
        .build)
    }

    override def startSimulation(file: File): Try[SimulationController] = {
      YamlLoader.loadCompleteSimulation(file) match {
        case Success(value) =>
          Success(new SimulationBuilder[EmptySimulation]
            .dimension(500, 500)
            .data(value)
            .build)
        case Failure(exception) =>
          //TODO completebuildfailedexception or something
          Failure(exception)
      }
    }

    override def loadSimulation(file: File): Try[PartialSimulationData] = {
      //TODO move to loader
      Try(YamlLoader.loadSimulation(file))
    }

    override def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Try[Unit] = {
      saver = Some(YamlSaver(simulation, simulationName))
      saver.get.saveData(target, false)
    }

    override def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Try[Unit] = {
      saver match {
        case Some(s) =>
          overrideResource.foreach(s.addResourceToOverride)
          s.saveData(target, overrideAll)
        case None =>
          Failure(new IllegalStateException())
      }
    }
  }
}

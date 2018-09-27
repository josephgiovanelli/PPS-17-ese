package it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators

import it.unibo.pps.ese.controller.simulation.loader.{FileSaver, YamlLoader, YamlSaver}
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.io.{ExistingResource, File, Folder}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.SimulationBuilder
import it.unibo.pps.ese.controller.simulation.runner.incarnation.SimulationBuilder.Simulation.EmptySimulation

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Base controller that offer services before simulation start*/
trait Controller {
  /** Start a new simulation starting from configuration file
    *
    * @param file Configuration file
    * @return Simulation controller that offer services after the simulation start and simulation data loaded from file
    */
  def startSimulation(file: File): Future[Try[(SimulationController, CompleteSimulationData)]]

  /** Start a new simulation starting from simulation;s data
    *
    * @param data Simulation's data
    * @return Simulation controller that offer services after the simulation start
    */
  def startSimulation(data: CompleteSimulationData): Future[Try[SimulationController]]

  /** Load simulation's data from given file
    *
    * @param file Simulation's config file
    * @return Simulation's setup data, loaded as partial
    */
  def loadSimulation(file: File): Future[Try[PartialSimulationData]]

  /** Try to save simulation in given folder
    *
    * @param simulation Simulation data
    * @param simulationName Simulation name
    * @param target Target folder
    * @return Operation result
    */
  def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Future[Try[Unit]]

  /** Retrive last simulation's data save
    *
    * @param target Target folder
    * @param overrideResource Optional resource to override
    * @param overrideAll Override all existing resources flag
    * @return
    */
  def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Future[Try[Unit]]
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators.Controller]] */
object Controller {

  /**
    * @return New [[it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators.Controller]]
    */
  def apply()(implicit executionContext: ExecutionContext): Controller = new BaseController()

  private class BaseController()(implicit executionContext: ExecutionContext) extends Controller {

    var saver: Option[FileSaver] = None

    override def startSimulation(data: CompleteSimulationData): Future[Try[SimulationController]] = {
      Future(Success(new SimulationBuilder[EmptySimulation]
        .dimension(500, 500)
        .data(data)
        .build))
    }

    override def startSimulation(file: File): Future[Try[(SimulationController, CompleteSimulationData)]] = {
      Future(YamlLoader.loadCompleteSimulation(file) match {
        case Success(value) =>
          Success((new SimulationBuilder[EmptySimulation]
            .dimension(500, 500)
            .data(value)
            .build, value))
        case Failure(exception) =>
          Failure(exception)
      })
    }

    override def loadSimulation(file: File): Future[Try[PartialSimulationData]] = {
      Future(Try(YamlLoader.loadSimulation(file)))
    }

    override def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Future[Try[Unit]] = {
      Future({
        saver = Some(YamlSaver(simulation, simulationName))
        saver.get.saveData(target, false)
      })
    }

    override def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Future[Try[Unit]] = {
      Future(saver match {
        case Some(s) =>
          overrideResource.foreach(s.addResourceToOverride)
          s.saveData(target, overrideAll)
        case None =>
          Failure(new IllegalStateException())
      })
    }
  }
}

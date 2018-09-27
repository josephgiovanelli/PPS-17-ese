package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.io.File

import scala.util.Try

/** Trait that defines generic simulation's data loader*/
trait Loader {
  type DataSource

  /** Method loads partial simulation data starting from given data source
    *
    * @param source Data source
    * @return Simulation setup data as partial instance
    */
  def loadSimulation(source: DataSource): PartialSimulationData

  /** Method tries to loads complete simulation data starting from given data source
    *
    * @param source Data source
    * @return Simulation setup data as partial instance
    */
  def loadCompleteSimulation(source: DataSource): Try[CompleteSimulationData]
}

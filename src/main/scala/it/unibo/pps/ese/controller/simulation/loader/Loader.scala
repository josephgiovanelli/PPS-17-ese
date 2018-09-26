package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.io.File

import scala.util.Try

trait Loader {
  type DataSource

  def loadSimulation(configFile: DataSource): PartialSimulationData
  def loadCompleteSimulation(configFile: DataSource): Try[CompleteSimulationData]
}

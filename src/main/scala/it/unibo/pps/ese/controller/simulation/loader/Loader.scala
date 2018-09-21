package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.io.File

import scala.util.Try

trait Loader {
  def loadSimulation(configFile: File): PartialSimulationData
  def loadCompleteSimulation(configFile: File): Try[CompleteSimulationData]
}

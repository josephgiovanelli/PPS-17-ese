package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.util.io.File

import scala.util.Try

trait Loader {
  def loadSimulation(configFile: File): PartialSimulationData
  def loadCompleteSimulation(configFile: File): Try[CompleteSimulationData]
}

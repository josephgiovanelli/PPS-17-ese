package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.util.io.File

trait Loader {
  def loadSimulation(configFile: File): PartialSimulationData
}

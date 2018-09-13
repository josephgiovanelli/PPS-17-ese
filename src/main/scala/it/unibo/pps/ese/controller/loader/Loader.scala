package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData

trait Loader {
  def loadSimulation(configPath: String): CompleteSimulationData
}

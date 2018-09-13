package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}

trait Loader {
  def loadSimulation(configPath: String): PartialSimulationData
}

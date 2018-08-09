package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.SimulationData

trait Loader {
  def loadSimulation(configPath: String): SimulationData
}

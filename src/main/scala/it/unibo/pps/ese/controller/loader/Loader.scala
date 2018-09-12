package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.loader.data.{CustomGeneData, DefaultGeneData, SimulationData}

trait Loader {
  def loadSimulation(configPath: String): SimulationData[CompleteAnimalData]
}

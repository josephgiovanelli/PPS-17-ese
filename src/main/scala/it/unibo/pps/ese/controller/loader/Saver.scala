package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData

trait Saver {
  def saveData(path: String, simulationData: PartialSimulationData)
}

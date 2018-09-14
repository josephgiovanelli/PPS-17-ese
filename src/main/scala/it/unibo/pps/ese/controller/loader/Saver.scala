package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.util.io.Folder

trait Saver {
  def simulationData: PartialSimulationData
  def simulationName: String
  def saveData(saveLocation: Folder, overrideAll: Boolean)
}

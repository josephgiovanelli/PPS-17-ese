package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.util.io.{ExistingResource, Folder, IOResource}

import scala.util.Try

trait Saver {
  def simulationData: PartialSimulationData
  def simulationName: String
  def saveData(saveLocation: Folder, overrideAll: Boolean): Try[Unit]
  def addResourceToOverride(resource: ExistingResource)
}

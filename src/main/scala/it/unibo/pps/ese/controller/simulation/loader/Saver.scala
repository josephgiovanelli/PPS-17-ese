package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.PartialSimulationData

import scala.util.Try

/** Trait that defines generic simulation's saver that save simulation setup data in a given memory support*/
trait Saver {
  type MemorySupport

  /** Data to save*/
  def simulationData: PartialSimulationData

  /** Method to save data in a given memory support
    *
    * @param saveLocation Support where data have to been saved
    * @param overrideAll If true saver overrides, if present, old data inside support
    * @return Save result
    */
  def saveData(saveLocation: MemorySupport, overrideAll: Boolean): Try[Unit]
}

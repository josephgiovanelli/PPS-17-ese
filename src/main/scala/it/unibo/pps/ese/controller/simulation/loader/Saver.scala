package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.io.ExistingResource

import scala.util.Try

trait Saver {
  type DataSupport
  def saveData(saveLocation: DataSupport, overrideAll: Boolean): Try[Unit]
}

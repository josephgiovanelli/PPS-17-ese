package it.unibo.pps.ese.controller.loader
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.util.io.{Folder, IOResource, UndefinedNotExistingResource}

object YamlSaver extends Saver {
  override def saveData(path: String, simulationData: PartialSimulationData): Unit = {
    IOResource(path) match {
      case f: Folder =>
      case r: UndefinedNotExistingResource =>
    }
  }
}

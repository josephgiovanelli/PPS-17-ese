package it.unibo.pps.ese.controller.simulation.loader.exception

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.PartialSimulationData

case class PartialSimulationDataException(partialSimulationData: PartialSimulationData) extends Exception {

}

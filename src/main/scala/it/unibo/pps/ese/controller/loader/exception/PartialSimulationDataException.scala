package it.unibo.pps.ese.controller.loader.exception

import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData

case class PartialSimulationDataException(partialSimulationData: PartialSimulationData) extends Exception {

}

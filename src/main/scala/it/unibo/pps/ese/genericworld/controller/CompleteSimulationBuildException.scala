package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData

case class CompleteSimulationBuildException(partialSimulationData: PartialSimulationData) extends Exception{

}

package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.PartialSimulationData

case class CompleteSimulationBuildException(partialSimulationData: PartialSimulationData, buildException: CompleteBuildException) extends Exception{

}

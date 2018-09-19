package it.unibo.pps.ese.controller.loader.data.builder.exception

import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData

case class CompleteSimulationBuildException(partialSimulationData: PartialSimulationData, buildException: CompleteBuildException) extends Exception{

}

package it.unibo.pps.ese.controller.loader.exception

import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException

case class CompleteSimulationBuildException(partialSimulationData: PartialSimulationData, buildException: CompleteBuildException) extends Exception{

}

package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.PartialSimulationData

class CompleteSimulationBuildException(motivations: Iterable[Motivation], val partialSimulationData: PartialSimulationData) extends CompleteBuildException(motivations) {
  def this(buildException: CompleteBuildException, partialSimulationData: PartialSimulationData) {
    this(buildException.motivations, partialSimulationData)
  }

  def this(motivation: String, partialSimulationData: PartialSimulationData) {
    this(new CompleteBuildException(motivation), partialSimulationData)
  }
}

package it.unibo.pps.ese.controller.simulation.loader.data.builder.exception

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.PartialSimulationData

/** [[it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException]] specialization
  * that indicates problems with a try of complete build of
  * [[it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder]]
  *
  * @param motivations Motivation
  * @param partialSimulationData Partial simulation data resulting from complete build try
  */
class CompleteSimulationBuildException(motivations: Iterable[Motivation], val partialSimulationData: PartialSimulationData)
  extends AbsHierarchyMotivationsException(motivations) with CompleteBuildException {

  /**
    * @param buildException Build exception
    * @param partialSimulationData Partial simulation data resulting from complete build try
    */
  def this(buildException: CompleteBuildException, partialSimulationData: PartialSimulationData) {
    this(buildException.motivations, partialSimulationData)
  }

  /**
    * @param motivation Motivation
    * @param partialSimulationData Partial simulation data resulting from complete build try
    */
  def this(motivation: String, partialSimulationData: PartialSimulationData) {
    this(CompleteBuildException(motivation), partialSimulationData)
  }
}

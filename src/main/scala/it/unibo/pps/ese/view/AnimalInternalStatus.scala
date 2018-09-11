package it.unibo.pps.ese.view

import it.unibo.pps.ese.entitybehaviors.EmbryoStatus

sealed trait Reason
case object Evaluating extends Reason
case object Coupling extends Reason
case object Eating extends Reason

sealed trait BrainStatus
case class HippoCampusActive(reason:Reason) extends BrainStatus
case object HippoCampusDisabled extends BrainStatus

sealed trait EyesStatus
case class EyesActive(reason:Reason) extends EyesStatus
case object EyesDisabled extends EyesStatus

sealed trait ReproductiveApparatusStatus
case object Reproducing extends ReproductiveApparatusStatus
case object NotReproducing extends ReproductiveApparatusStatus

sealed trait DigestiveSystemStatus
case object Digesting extends DigestiveSystemStatus
case object NotDigesting extends DigestiveSystemStatus

sealed trait  AnimalInternalStatus {
  def brainStatus:BrainStatus
  def eyesStatus:EyesStatus
  def reproductiveApparatusStatus:ReproductiveApparatusStatus
  def digestiveSystemStatus:DigestiveSystemStatus
}
sealed trait FemaleInternalStatus extends AnimalInternalStatus{
  def fetusStatus:Option[EmbryoStatus.Value]
}
  object AnimalInternalStatus{
    def apply(brainStatus: BrainStatus,
              eyesStatus: EyesStatus,
              reproductiveApparatusStatus: ReproductiveApparatusStatus,
              digestiveSystemStatus:DigestiveSystemStatus
             ): AnimalInternalStatus =
      AnimalInternalStatusImpl(brainStatus,eyesStatus,reproductiveApparatusStatus,digestiveSystemStatus)

    private case class AnimalInternalStatusImpl(
                                                 brainStatus: BrainStatus,
                                                 eyesStatus: EyesStatus,
                                                 reproductiveApparatusStatus: ReproductiveApparatusStatus,
                                                 digestiveSystemStatus:DigestiveSystemStatus
                                               ) extends AnimalInternalStatus
  }
  object FemaleInternalStatus{
    def apply(brainStatus: BrainStatus,
              eyesStatus: EyesStatus,
              reproductiveApparatusStatus: ReproductiveApparatusStatus,
              digestiveSystemStatus:DigestiveSystemStatus,
              fetusStatus:Option[EmbryoStatus.Value]): FemaleInternalStatus =
      FemaleInternalStatusImpl(brainStatus,eyesStatus,reproductiveApparatusStatus,digestiveSystemStatus,fetusStatus)
    
    private case class FemaleInternalStatusImpl(brainStatus: BrainStatus,
                                                eyesStatus: EyesStatus,
                                                reproductiveApparatusStatus: ReproductiveApparatusStatus,
                                                digestiveSystemStatus:DigestiveSystemStatus,
                                                fetusStatus:Option[EmbryoStatus.Value])
      extends FemaleInternalStatus
}


package it.unibo.pps.ese.view.bodyViewer

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

/**
  * Trait to obtain the internal status of an Animal
  */
sealed trait  AnimalInternalStatus {
  def brainStatus:BrainStatus
  def eyesStatus:EyesStatus
  def reproductiveApparatusStatus:ReproductiveApparatusStatus
  def digestiveSystemStatus:DigestiveSystemStatus
}

/**
  * Case class describing the internal status of a Male Animal
  * @param brainStatus
  * @param eyesStatus
  * @param reproductiveApparatusStatus
  * @param digestiveSystemStatus
  */
case class MaleInternalStatus(
                               brainStatus: BrainStatus,
                               eyesStatus: EyesStatus,
                               reproductiveApparatusStatus: ReproductiveApparatusStatus,
                               digestiveSystemStatus:DigestiveSystemStatus
                             )extends AnimalInternalStatus

/**
  * Case class describing the internal status of a Female Animal
  * @param brainStatus
  * @param eyesStatus
  * @param reproductiveApparatusStatus
  * @param digestiveSystemStatus
  * @param fetusStatus
  */
case class FemaleInternalStatus(
                                 brainStatus: BrainStatus,
                                 eyesStatus: EyesStatus,
                                 reproductiveApparatusStatus: ReproductiveApparatusStatus,
                                 digestiveSystemStatus:DigestiveSystemStatus,
                                 fetusStatus:Option[EmbryoStatus.Value]
                               ) extends AnimalInternalStatus


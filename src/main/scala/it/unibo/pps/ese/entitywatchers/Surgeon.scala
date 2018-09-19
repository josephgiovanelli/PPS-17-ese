package it.unibo.pps.ese.entitywatchers

import it.unibo.pps.ese.entitybehaviors.ActionKind
import it.unibo.pps.ese.entitybehaviors.decisionsupport.SexTypes
import it.unibo.pps.ese.genericworld.model.{EntityInfoConversion, EntityState, ReadOnlyEntityState, ReignType}
import it.unibo.pps.ese.view._
import it.unibo.pps.ese.view.bodyViewer._

case class Surgeon(realTimeState: ReadOnlyEntityState) {

  var inspected: Option[String] = None

  def inspects(entityId: String): Unit = synchronized {
    inspected = Some(entityId)
  }

  def leaves(): Unit = synchronized {
    inspected = None
  }

  def informAboutOrgansStatus(view: View): Unit = synchronized {
    if(inspected.isDefined) {
      import EntityInfoConversion._
      val entity: Option[EntityState] = realTimeState getFilteredState(x => x.entityId == inspected.get) match {
        case Seq(single) => Some(single)
        case _ => None
      }
      if (entity.isDefined && entity.get.state.reign == ReignType.ANIMAL) {
        val state = entity.get.state
        val reason: Reason = if (state.will == ActionKind.EAT) Eating else if (state.will == ActionKind.COUPLE) Coupling else Evaluating
        val brainStatus: BrainStatus = if (state.hippocampus) HippoCampusActive(reason) else HippoCampusDisabled
        val eyesStatus: EyesStatus = if (state.eyes) EyesActive(reason) else EyesDisabled
        val reproductiveApparatusStatus: ReproductiveApparatusStatus = if (state.reproductionOrgan) Reproducing else NotReproducing
        val digestiveSystemStatus: DigestiveSystemStatus = if (state.stomach) Digesting else NotDigesting

        var animalInternalStatus: AnimalInternalStatus =
          MaleInternalStatus(brainStatus, eyesStatus, reproductiveApparatusStatus, digestiveSystemStatus)

        if (state.gender == SexTypes.female)
          animalInternalStatus =
            FemaleInternalStatus(brainStatus, eyesStatus, reproductiveApparatusStatus, digestiveSystemStatus, state.embryo)

        view.updateAnimalInternalStatus(animalInternalStatus)
      }
    }
  }

}

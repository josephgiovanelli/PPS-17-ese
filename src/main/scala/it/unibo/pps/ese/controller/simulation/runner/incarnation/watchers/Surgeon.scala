package it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers

import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.SexTypes
import it.unibo.pps.ese.controller.simulation.runner.core.{EntityState, ReadOnlyEntityState}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.components.animals.brain.ActionKind
import it.unibo.pps.ese.view._
import it.unibo.pps.ese.view.sections.bodyviewer._
import it.unibo.pps.ese.view.core.View

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
      import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
      val entity: Option[EntityState] = realTimeState getFilteredState(x => x.entityId == inspected.get) match {
        case Seq(single) => Some(single)
        case _ => None
      }
      if (entity.isDefined && entity.get.state.reign == ReignType.ANIMAL) {
        val state = entity.get.state
        val reason: Reason = if (state.will == ActionKind.EAT) Eating else if (state.will == ActionKind.COUPLE) Coupling else Evaluating
        val brainStatus: BrainStatus = if (state.hippocampus) HippoCampusActive(reason) else HippoCampusDisabled(reason)
        val eyesStatus: EyesStatus = if (state.eyes) EyesActive(reason) else EyesDisabled(reason)
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

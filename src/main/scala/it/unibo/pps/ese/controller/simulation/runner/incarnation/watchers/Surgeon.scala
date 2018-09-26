package it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers

import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.GenderTypes
import it.unibo.pps.ese.controller.simulation.runner.core.data.{EntityState, ReadOnlyEntityState}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.components.animals.brain.{Couple, Eat, Nothing}
import it.unibo.pps.ese.view.sections.bodyviewer._
import it.unibo.pps.ese.view.core.View

/**
  * Component that inspect an entity in order to know the internal status of it.
  * @param realTimeState the repository to query
  */
case class Surgeon(realTimeState: ReadOnlyEntityState) {

  /**
    * The entity identifier to inspect.
    */
  var inspected: Option[String] = None

  /**
    * It allows to inspect the entity.
    * @param entityId the entity identifier
    */
  def inspects(entityId: String): Unit = this synchronized {
    inspected = Some(entityId)
  }

  /**
    * It allows to leaves the entity.
    */
  def leaves(): Unit = this synchronized {
    inspected = None
  }

  /**
    * It queries the repository and inform the view about the entity organs activity.
    * @param view the view of the system
    */
  def informAboutOrgansStatus(view: View): Unit = this synchronized {
    if(inspected.isDefined) {
      import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
      val entity: Option[EntityState] = realTimeState getFilteredState(x => x.entityId == inspected.get) match {
        case Seq(single) => Some(single)
        case _ => None
      }
      if (entity.isDefined && entity.get.state.reign == ReignType.ANIMAL) {
        val state = entity.get.state
        val reason: Reason = state.will match {
          case Eat => Eating
          case Couple => Coupling
          case Nothing => Evaluating
        }
        val brainStatus: BrainStatus = if (state.hippocampus) HippoCampusActive(reason) else HippoCampusDisabled(reason)
        val eyesStatus: EyesStatus = if (state.eyes) EyesActive(reason) else EyesDisabled(reason)
        val reproductiveApparatusStatus: ReproductiveApparatusStatus = if (state.reproductionOrgan) Reproducing else NotReproducing
        val digestiveSystemStatus: DigestiveSystemStatus = if (state.stomach) Digesting else NotDigesting

        var animalInternalStatus: AnimalInternalStatus =
          MaleInternalStatus(brainStatus, eyesStatus, reproductiveApparatusStatus, digestiveSystemStatus)

        if (state.gender == GenderTypes.female)
          animalInternalStatus =
            FemaleInternalStatus(brainStatus, eyesStatus, reproductiveApparatusStatus, digestiveSystemStatus, state.embryo)

        view.updateAnimalInternalStatus(animalInternalStatus)
      }
    }
  }

}

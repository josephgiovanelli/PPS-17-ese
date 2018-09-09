package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.support.BaseEvent

import scala.concurrent.ExecutionContext


case class OrgansTrackerInfo(eyes: Boolean,
                             hippocampus: Boolean,
                             stomach: Boolean,
                             pregnant: Boolean,
                             embryo: Option[EmbryoStatus.Value],
                             reproductionOrgan: Boolean) extends BaseEvent

case class ClearOrgans() extends BaseEvent
case class ActivateEyes() extends BaseEvent
case class ActivateHippocampus() extends BaseEvent
case class ActivateStomach() extends BaseEvent
case class DeactivateStomach() extends BaseEvent
case class ActivateReproductionOrgan() extends BaseEvent
case class InitPregnancy() extends BaseEvent
case class GrowEmbryo() extends BaseEvent
case class EndPregnancy() extends BaseEvent

object EmbryoStatus extends Enumeration {
  val primal, mid, advanced = Value
}

class OrgansTrackerComponent(override val entitySpecifications: EntitySpecifications)
                                  (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications)  {

  var eyes: Boolean = false
  var hippocampus: Boolean = false
  var stomach: Boolean = false
  var pregnant: Boolean = false
  var embryo: Option[EmbryoStatus.Value] = None
  var reproductionOrgan: Boolean = false

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        eyes = false
        hippocampus = false
        reproductionOrgan = false
        publish(ClearOrgans())
        publish(new ComputeNextStateAck)

      case UseEyes() =>
        eyes = true
        publish(ActivateEyes())
      case UseHippocampus() =>
        hippocampus = true
        publish(ActivateHippocampus())

      case InteractionEntity(_, action) =>
        if (action == ActionKind.EAT) {
          stomach = true
          publish(ActivateStomach())
        } else {
          reproductionOrgan = true
          publish(ActivateReproductionOrgan())
        }
      case DigestionEnd() =>
        stomach = false
        publish(DeactivateStomach())

      case Pregnant() =>
        pregnant = true
        embryo = Some(EmbryoStatus.primal)
        publish(InitPregnancy())
      case PregnancyRequirements(_) =>
        embryo = Some(EmbryoStatus(embryo.get.id + 1))
        publish(GrowEmbryo())
      case _: PregnancyEnd =>
        pregnant = false
        embryo = None
        publish(EndPregnancy())

      case GetInfo() =>
        publish(OrgansTrackerInfo(eyes, hippocampus, stomach, pregnant, embryo, reproductionOrgan))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  private def configureMappings(): Unit = {
    addMapping[OrgansTrackerInfo]((classOf[OrgansTrackerInfo], ev => Seq(
      EntityProperty("eyes", ev eyes),
      EntityProperty("hippocampus", ev hippocampus),
      EntityProperty("stomach", ev stomach),
      EntityProperty("pregnant", ev pregnant),
      EntityProperty("embryo", ev embryo),
      EntityProperty("reproductionOrgan", ev reproductionOrgan)
    )))

    addMapping[ClearOrgans]((classOf[ClearOrgans], _ => Seq(
      EntityProperty("eyes", eyes),
      EntityProperty("hippocampus", hippocampus),
      EntityProperty("reproductionOrgan", reproductionOrgan)
    )))

    addMapping[ActivateEyes]((classOf[ActivateEyes], _ => Seq(
      EntityProperty("eyes", eyes)
    )))

    addMapping[ActivateHippocampus]((classOf[ActivateHippocampus], _ => Seq(
      EntityProperty("hippocampus", hippocampus)
    )))

    addMapping[ActivateStomach]((classOf[ActivateStomach], _ => Seq(
      EntityProperty("stomach", stomach)
    )))

    addMapping[DeactivateStomach]((classOf[DeactivateStomach], _ => Seq(
      EntityProperty("stomach", stomach)
    )))

    addMapping[ActivateReproductionOrgan]((classOf[ActivateReproductionOrgan], _ => Seq(
      EntityProperty("reproductionOrgan", reproductionOrgan)
    )))

    addMapping[InitPregnancy]((classOf[InitPregnancy], _ => Seq(
      EntityProperty("pregnant", pregnant),
      EntityProperty("embryo", embryo)
    )))

    addMapping[GrowEmbryo]((classOf[GrowEmbryo], _ => Seq(
      EntityProperty("embryo", embryo)
    )))

    addMapping[EndPregnancy]((classOf[EndPregnancy], _ => Seq(
      EntityProperty("pregnant", pregnant),
      EntityProperty("embryo", embryo)
    )))

    addMapping[NewMutantAlleles]((classOf[NewMutantAlleles], ev => Seq(
      EntityProperty("genes", ev.mutantGenes.map(x => x.toString))
    )))
  }

}

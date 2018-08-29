package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genericworld.model.support.{BaseEvent, InteractionEvent, RequestEvent}
import it.unibo.pps.ese.genetics.dna.{AnimalGenome, Genome}

case class ReproductionBaseInformationRequest() extends RequestEvent
case class ReproductionBaseInformationResponse(gender: String) extends RequestEvent
case class PhysicalBaseInformationRequest() extends RequestEvent
case class PhysicalBaseInformationResponse(fertility: Double, age: Double) extends RequestEvent
case class PartnerInfoRequest(partnerId: String) extends InteractionEvent(partnerId)
case class PartnerInfoResponse(partnerId: String, partnerGenome: AnimalGenome, partnerFertility: Double) extends InteractionEvent(partnerId)

case class ReproductionComponent(override val entitySpecifications: EntitySpecifications,
                                 fecundity: Double,
                                 geneticsSimulator: GeneticsSimulator,
                                 animalGenome: AnimalGenome
                                  ) extends WriterComponent(entitySpecifications)  {

  override def initialize(): Unit = {
    subscribeEvents()
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        publish(new ComputeNextStateAck)
      case InteractionEntity(id, kind) if kind == ActionKind.COUPLE =>
        geneticsSimulator
      case GetInfo() =>
        publish(new GetInfoAck)
      case _ => Unit
    }
  }
}

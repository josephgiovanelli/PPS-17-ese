package it.unibo.pps.ese.model.components.animals.reproduction

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.BaseEvent
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.model.components.{BaseInfoRequest, BaseInfoResponse}
import it.unibo.pps.ese.model.components.animals.brain.{Couple, InteractionEntity}
import it.unibo.pps.ese.utils.Point

import scala.concurrent.ExecutionContext

case class FakeStatusInfo(species: String, status: EntityUpdateState.Value) extends BaseEvent

case class FakeComponent(override val entitySpecifications: EntitySpecifications,
                         species: String,
                         gender: String,
                         position: Point,
                         fertility: Double,
                         var partner: Option[String])
                        (implicit val executionContext: ExecutionContext)
  extends WriterComponent(entitySpecifications) {
  var forceReproduction: Option[ForceReproduction] = None

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case ComputeNextState() =>
      if(partner.nonEmpty) {
        publish(InteractionEntity(partner.get, Couple))
        partner = None
      } else if(forceReproduction.nonEmpty) {
        publish(ForceReproductionForward(forceReproduction.get))
        forceReproduction = None
      }
      publish(new ComputeNextStateAck)
    case r: BaseInfoRequest =>
      publish(BaseInfoResponse(r.id, species, null, position, 0, 0, 0, "", null))
    case r: ReproductionBaseInformationRequest =>
      publish(ReproductionBaseInformationResponse(r.id, gender, species))
    case r: ReproductionPhysicalInformationRequest =>
      publish(ReproductionPhysicalInformationResponse(r.id, fertility))
    case r: AutoForceReproduction =>
      forceReproduction = Some(r)
    case r: PartnerForceReproduction if r.receiverId == entitySpecifications.id =>
      forceReproduction = Some(r)
    case GetInfo() =>
      this synchronized {
        publish(FakeStatusInfo(species, EntityUpdateState.WAITING))
      }
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[FakeStatusInfo]((classOf[FakeStatusInfo], ev => Seq(
      EntityProperty("species", ev.species),
      EntityProperty("status", ev.status)
    )))
  }
}

package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.decisionsupport.SexTypes
import it.unibo.pps.ese.entitybehaviors.util.reproduction.{EmbryosUtil, GeneticsEngine, ReproductionInfo}
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genericworld.model.support.{InteractionEvent, RequestEvent, ResponseEvent}
import it.unibo.pps.ese.genetics.dna.{AnimalGenome, MGene}
import it.unibo.pps.ese.genetics.entities.AnimalInfo

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

case class ReproductionBaseInformationRequest() extends RequestEvent
case class ReproductionBaseInformationResponse(override val id: String, gender: String, elapsedClocks: Long, species: String) extends ResponseEvent
case class PhysicalBaseInformationRequest() extends RequestEvent
case class PhysicalBaseInformationResponse(override val id: String, fertility: Double, age: Double) extends ResponseEvent
case class PartnerInfoRequest(override val receiverId: String) extends InteractionEvent with RequestEvent
case class PartnerInfoResponse(override val id: String, override val receiverId: String, partnerGenome: AnimalGenome,
                               partnerFertility: Double) extends InteractionEvent with ResponseEvent

case class ReproductionComponent(override val entitySpecifications: EntitySpecifications,
                                 fecundity: Double,
                                 geneticsSimulator: GeneticsSimulator,
                                 animalGenome: AnimalGenome,
                                 clockPerYear: Long,
                                 pregnancyDuration: Long,
                                 mutationProb: Double
                                  ) extends WriterComponent(entitySpecifications)  {

  var embryos: Seq[AnimalInfo] = Seq()
  var inPregnancyTime: Long = 0

  implicit val geneticEngine: GeneticsEngine = GeneticsEngine(geneticsSimulator, mutationProb)

  override def initialize(): Unit = {
    subscribeEvents()
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        if(embryos.nonEmpty) {
          inPregnancyTime += 1
          if(inPregnancyTime >= pregnancyDuration) {
            publish(Create(embryos))
            embryos = Seq()
          }
        }
        publish(new ComputeNextStateAck)
      case InteractionEntity(id, kind) if kind == ActionKind.COUPLE =>
        val f1 = requireData[EntitiesStateRequest, EntitiesStateResponse](EntitiesStateRequest(x => x.entityId == id))
        val f2 = requireData[ReproductionBaseInformationRequest, ReproductionBaseInformationResponse](ReproductionBaseInformationRequest())
        val result = for {
          r1 <- f1
          r2 <- f2
        } yield (r1, r2)

        result.onComplete{
          case Success((partnerBaseInfo, myBaseInfo)) =>
            import EntityInfoConversion._
            if(partnerBaseInfo.state.nonEmpty) {
              if (partnerBaseInfo.state.head.state.elapsedClock < myBaseInfo.elapsedClocks)
                println("Di all'altro e a me stesso di copulare alla prossima (check che sia ancora vivo)")
              else
              if(embryos.isEmpty) {
                copulate(id, myBaseInfo.gender, myBaseInfo.species, partnerBaseInfo.state.head.state.species.toString)
              }
            }
          case Failure(error) => throw error
        }
      case GetInfo() =>
        publish(new GetInfoAck)
      case _ => Unit
    }
  }
  //TODO salvataggio interno specie
  private def copulate(partnerId: String, gender: String, species: String, partnerSpecies: String): Unit = {
    if(SexTypes.withNameOpt(gender).get == SexTypes.female) {
      val f1 = requireData[PartnerInfoRequest, PartnerInfoResponse](PartnerInfoRequest(partnerId))
      val f3 = requireData[PhysicalBaseInformationRequest, PhysicalBaseInformationResponse](PhysicalBaseInformationRequest())
      val result = for {
        r1 <- f1
        r2 <- f3
      } yield (r1, r2)
      result.onComplete {
        case Success((partner, physycal)) =>
          embryos = EmbryosUtil.createEmbryos(ReproductionInfo(animalGenome, physycal.fertility, species),
            ReproductionInfo(partner.partnerGenome, partner.partnerFertility, partnerSpecies), fecundity)
        case Failure(error) => throw error
      }
    }
  }

}

package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.decisionsupport.SexTypes
import it.unibo.pps.ese.entitybehaviors.util.reproduction.{EmbryosUtil, GeneticsEngine, ReproductionInfo}
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genericworld.model.support.{BaseEvent, InteractionEvent, RequestEvent, ResponseEvent}
import it.unibo.pps.ese.genetics.dna.AnimalGenome
import it.unibo.pps.ese.genetics.entities.AnimalInfo

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

case class ReproductionBaseInformationRequest() extends RequestEvent
case class ReproductionBaseInformationResponse(override val id: String, gender: String, elapsedClocks: Long, species: String) extends ResponseEvent
case class ReproductionPhysicalInformationRequest() extends RequestEvent
case class ReproductionPhysicalInformationResponse(override val id: String, fertility: Double, age: Double) extends ResponseEvent
case class PartnerInfoRequest(override val receiverId: String, senderId: String) extends InteractionEvent with RequestEvent
case class PartnerInfoResponse(override val id: String, override val receiverId: String, partnerGenome: AnimalGenome,
                               partnerFertility: Double) extends InteractionEvent with ResponseEvent

case class PregnancyRequirements(extraEnergyRequirements: Double) extends BaseEvent
case class PregnancyEnd() extends BaseEvent

case class ReproductionComponent(override val entitySpecifications: EntitySpecifications,
                                 fecundity: Double,
                                 geneticsSimulator: GeneticsSimulator,
                                 animalGenome: AnimalGenome,
                                 clockPerYear: Long,
                                 pregnancyDuration: Long,
                                 mutationProb: Double,
                                 energyRequirements: Double
                                  ) extends WriterComponent(entitySpecifications)  {

  private val energyRequirementsPerChild = energyRequirements * 0.2 / math.round(energyRequirements)
  var embryos: Seq[AnimalInfo] = Seq()
  var inPregnancyTime: Long = 0

  implicit val geneticEngine: GeneticsEngine = GeneticsEngine(geneticsSimulator, mutationProb)

  override def initialize(): Unit = {
    subscribeEvents()
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        //println("Hey, compute")
        if(embryos.nonEmpty) {
          println("Pregnant")
          inPregnancyTime += 1
          //TODO find a clever way
          if(inPregnancyTime == pregnancyDuration / 3)
            publish(PregnancyRequirements(energyRequirementsPerChild * embryos.size / 2))
          if(inPregnancyTime == pregnancyDuration / 3 * 2)
            publish(PregnancyRequirements(energyRequirementsPerChild * embryos.size / 2))
          if(inPregnancyTime >= pregnancyDuration) {
            embryos = Seq()
            publish(PregnancyEnd())
            //TODO death possible?
            println("Childbirth")
            publish(Create(embryos))
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
              if (partnerBaseInfo.state.head.state.elapsedClocks < myBaseInfo.elapsedClocks)
                print("")
                //TODO force me and other animal to copulate at next move
              else
              if(embryos.isEmpty) {
                //TODO force other entity to copulate
                copulate(id, myBaseInfo.gender, myBaseInfo.species, partnerBaseInfo.state.head.state.species.toString)
              }
            }
          case Failure(error) => throw error
        }
      case r: PartnerInfoRequest =>
        requireData[ReproductionPhysicalInformationRequest, ReproductionPhysicalInformationResponse](ReproductionPhysicalInformationRequest())
          .onComplete{
            case Success(info) =>
              publish(PartnerInfoResponse(r.id, r.senderId, animalGenome, info.fertility))
            case Failure(exception) =>
              exception
          }
      case GetInfo() =>
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  private def copulate(partnerId: String, gender: String, species: String, partnerSpecies: String): Unit = {
    if(SexTypes.withNameOpt(gender).get == SexTypes.female) {
      val f1 = requireData[PartnerInfoRequest, PartnerInfoResponse](PartnerInfoRequest(partnerId, entitySpecifications.id))
      val f3 = requireData[ReproductionPhysicalInformationRequest, ReproductionPhysicalInformationResponse](ReproductionPhysicalInformationRequest())
      val result = for {
        r1 <- f1
        r2 <- f3
      } yield (r1, r2)
      result.onComplete {
        case Success((partner, physical)) =>
          embryos = EmbryosUtil.createEmbryos(ReproductionInfo(animalGenome, physical.fertility, species),
            ReproductionInfo(partner.partnerGenome, partner.partnerFertility, partnerSpecies), fecundity)
        case Failure(error) => throw error
      }
    }
  }

}

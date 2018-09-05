package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.decisionsupport.SexTypes
import it.unibo.pps.ese.entitybehaviors.util.reproduction.{EmbryosUtil, GeneticsEngine, ReproductionInfo}
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genericworld.model.support.{BaseEvent, InteractionEvent, RequestEvent, ResponseEvent}
import it.unibo.pps.ese.genetics.dna.AnimalGenome
import it.unibo.pps.ese.genetics.entities.AnimalInfo

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}



case class ReproductionBaseInformationRequest() extends RequestEvent
case class ReproductionBaseInformationResponse(override val id: String, gender: String, species: String) extends ResponseEvent
case class ReproductionPhysicalInformationRequest() extends RequestEvent
case class ReproductionPhysicalInformationResponse(override val id: String, fertility: Double) extends ResponseEvent
case class PartnerInfoRequest(override val receiverId: String, senderId: String) extends InteractionEvent with RequestEvent
case class PartnerInfoResponse(override val id: String, override val receiverId: String, partnerGenome: AnimalGenome,
                               partnerFertility: Double) extends InteractionEvent with ResponseEvent

case class PregnancyRequirements(extraEnergyRequirements: Double) extends BaseEvent
case class PregnancyEnd() extends BaseEvent

trait ForceReproduction
case class AutoForceReproduction(partnerId: String) extends ForceReproduction with BaseEvent
case class PartnerForceReproduction(override val receiverId: String, genome: AnimalGenome, fertility: Double, species: String)
  extends ForceReproduction with InteractionEvent with ReproductionInfo
case class ForceReproductionForward[T <: ForceReproduction](forwardedForcing: T) extends BaseEvent

case class ReproductionComponent(override val entitySpecifications: EntitySpecifications,
                                 fecundity: Double,
                                 geneticsSimulator: GeneticsSimulator,
                                 animalGenome: AnimalGenome,
                                 pregnancyDuration: Long,
                                 mutationProb: Double,
                                 energyRequirements: Double)
                                (implicit val executionContext: ExecutionContext)
                                  extends WriterComponent(entitySpecifications)  {

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
        //println("non-Pregnant")
        //TODO sync problem: if embryos created in this era?
        if(embryos.nonEmpty) {
          println("Pregnant")
          inPregnancyTime += 1
          //TODO find a clever way
          if(inPregnancyTime == pregnancyDuration / 3)
            publish(PregnancyRequirements(energyRequirementsPerChild * embryos.size / 2))
          if(inPregnancyTime == pregnancyDuration / 3 * 2)
            publish(PregnancyRequirements(energyRequirementsPerChild * embryos.size / 2))
          if(inPregnancyTime >= pregnancyDuration) {
            publish(Create(embryos))
            embryos = Seq()
            publish(PregnancyEnd())
            //TODO death possible?
            println("Childbirth")
          }
        }
        publish(new ComputeNextStateAck)
      case InteractionEntity(partnerId, kind) if kind == ActionKind.COUPLE =>
        //println("received")
        checkPartnerExistence(partnerId, partnerBaseInfo => {
          //println("partner exists")
          obtainPersonalData((myBaseInfo, myPhysicalInfo) => {
            //println(partnerBaseInfo.state.head.state.status)
            import EntityInfoConversion._
            if (partnerBaseInfo.state.head.state.status == EntityUpdateState.UPDATED) {
              //force me and other animal to copulate at next move
              //println("busy partner")
              publish(AutoForceReproduction(partnerId))
              publish(PartnerForceReproduction(partnerId, animalGenome, myPhysicalInfo.fertility, partnerBaseInfo.state.head.state.species.toString))
            } else if(embryos.isEmpty) {
              //println("free partner")
              //force other animal to copulate
              publish(PartnerForceReproduction(partnerId, animalGenome, myPhysicalInfo.fertility, partnerBaseInfo.state.head.state.species.toString))
              createEmbryos(partnerId, myBaseInfo.gender, myBaseInfo.species, partnerBaseInfo.state.head.state.species.toString,
                myPhysicalInfo.fertility)
            }
          })
        })
      case r: PartnerInfoRequest  /*if r.senderId != entitySpecifications.id*/=>
        //println("Received request by: ", entitySpecifications.id)
        requireData[ReproductionPhysicalInformationRequest, ReproductionPhysicalInformationResponse](ReproductionPhysicalInformationRequest())
          .onComplete{
            case Success(info) =>
              //println("Send response: ", entitySpecifications.id)
              publish(PartnerInfoResponse(r.id, r.senderId, animalGenome, info.fertility))
            case Failure(exception) =>
              exception
          }
      case r: ForceReproductionForward[_] =>
        handleForcedReproduction(r.forwardedForcing)
      case GetInfo() =>
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  private def handleForcedReproduction(forwardedForcing: ForceReproduction): Unit = forwardedForcing match {
    case AutoForceReproduction(partnerId) =>
      checkPartnerExistence(partnerId, partnerBaseInfo => {
        obtainPersonalData((myBaseInfo, myPhysicalInfo) => {
          createEmbryos(partnerId, myBaseInfo.gender, myBaseInfo.species, partnerBaseInfo.state.head.state.species.toString,
            myPhysicalInfo.fertility)
        })
      })
    case r: PartnerForceReproduction =>
      obtainPersonalData((myBaseInfo, myPhysicalInfo) => {
        createEmbryos(r, myBaseInfo.gender, myBaseInfo.species, myPhysicalInfo.fertility)
      })
  }

  private def checkPartnerExistence(partnerId: String, f: EntitiesStateResponse => Unit): Unit = {
    requireData[EntitiesStateRequest, EntitiesStateResponse](EntitiesStateRequest(x => x.entityId == partnerId))
      .onComplete{
        case Success(value) =>
          if(value.state.nonEmpty) {
            f(value)
          }
        case Failure(exception) =>
          exception
      }
  }

  private def obtainPersonalData(f: (ReproductionBaseInformationResponse, ReproductionPhysicalInformationResponse) => Unit): Unit = {
    val f2 = requireData[ReproductionBaseInformationRequest, ReproductionBaseInformationResponse](ReproductionBaseInformationRequest())
    val f3 = requireData[ReproductionPhysicalInformationRequest, ReproductionPhysicalInformationResponse](ReproductionPhysicalInformationRequest())
    val result: SupervisedFuture[(ReproductionBaseInformationResponse, ReproductionPhysicalInformationResponse)] = for {
      r2 <- f2
      r3 <- f3
    } yield (r2, r3)
    result.onComplete{
      case Success((myBaseInfo, myPhysicalInfo)) =>
          f(myBaseInfo, myPhysicalInfo)
      case Failure(error) => throw error
    }
  }

  private def createEmbryos(partnerId: String, gender: String, species: String, partnerSpecies: String, myFertility: Double): Unit = {
    if(SexTypes.withNameOpt(gender).get == SexTypes.female) {
      println("sending: ", entitySpecifications.id, " to ", partnerId)
      requireData[PartnerInfoRequest, PartnerInfoResponse](PartnerInfoRequest(partnerId, entitySpecifications.id))
      .onComplete {
        case Success(partner) =>
          //println("embryos created")
          embryos = EmbryosUtil.createEmbryos(ReproductionInfo(animalGenome, myFertility, species),
            ReproductionInfo(partner.partnerGenome, partner.partnerFertility, partnerSpecies), fecundity)
        case Failure(error) => throw error
      }
    }
  }

  private def createEmbryos(partner: ReproductionInfo, gender: String, species: String, myFertility: Double): Unit = {
    if (SexTypes.withNameOpt(gender).get == SexTypes.female) {
      embryos = EmbryosUtil.createEmbryos(ReproductionInfo(animalGenome, myFertility, species),
        partner, fecundity)
    }
  }
}

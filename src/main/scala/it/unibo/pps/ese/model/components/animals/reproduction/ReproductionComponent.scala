package it.unibo.pps.ese.model.components.animals.reproduction

import it.unibo.pps.ese.controller.simulation.runner.core
import it.unibo.pps.ese.model.components.animals.reproduction.util.{EmbryosUtil, GeneticsEngine, ReproductionInfo}
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.support._
import it.unibo.pps.ese.model.components._
import it.unibo.pps.ese.model.components.animals.brain.{Couple, InteractionEntity}
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.GenderTypes
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.dna.AnimalGenome
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo
import it.unibo.pps.ese.utils.Point

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

case class ReproductionBaseInformationRequest() extends RequestEvent
case class ReproductionBaseInformationResponse(override val id: String, gender: String, species: String) extends ResponseEvent
case class ReproductionPhysicalInformationRequest() extends RequestEvent
case class ReproductionPhysicalInformationResponse(override val id: String, fertility: Double) extends ResponseEvent
case class PartnerInfoRequest(override val receiverId: String, senderId: String) extends InteractionEvent with RequestEvent
case class PartnerInfoResponse(override val id: String, override val receiverId: String, partnerGenome: AnimalGenome,
                               partnerFertility: Double) extends InteractionEvent with ResponseEvent

/**
  * Message that indicates pregnancy start
  */
case class Pregnant() extends BaseEvent

/**
  * Message that indicates an extra energy requirements caused by fetus growing
  * @param extraEnergyRequirements extra energy requirements
  */
case class PregnancyRequirements(extraEnergyRequirements: Double) extends BaseEvent
/**
  * Message that indicates pregnancy end
  */
case class PregnancyEnd() extends BaseEvent

/*
 * Messages for sync partners' copulation
 */
trait ForceReproduction
case class AutoForceReproduction(partnerId: String) extends ForceReproduction with BaseEvent
case class PartnerForceReproduction(override val receiverId: String, genome: AnimalGenome, fertility: Double, species: String)
  extends ForceReproduction with InteractionEvent with ReproductionInfo
case class ForceReproductionForward[T <: ForceReproduction](forwardedForcing: T) extends BaseEvent

case class ReproductionComponent(override val entitySpecifications: EntitySpecifications,
                                 fecundity: Double,
                                 geneticsSimulator: GeneticsSimulator,
                                 animalGenome: AnimalGenome,
                                 pregnancyDuration: Double,
                                 clocksPerYear: Long,
                                 mutationProb: Double,
                                 energyRequirements: Double,
                                 animalCreationFunction: (AnimalInfo, Point) => Entity)
                                (implicit val executionContext: ExecutionContext)
                                  extends WriterComponent(entitySpecifications)  {

  private val energyRequirementsPerChild = energyRequirements * 0.2 / math.round(fecundity)
  private val pregnancyDurationInClocks: Long = (clocksPerYear * pregnancyDuration).toLong
  private var embryos: Seq[AnimalInfo] = Seq()
  private var inPregnancyTime: Long = 0
  private val energyRequirementsIncreaseSteps = 2
  private val energyRequirementsIncreasePeriod = pregnancyDurationInClocks / (energyRequirementsIncreaseSteps + 1)

  implicit val geneticEngine: GeneticsEngine = GeneticsEngine(geneticsSimulator, mutationProb)

  override def initialize(): Unit = {
    subscribeEvents()
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        if(embryos.nonEmpty) {
          inPregnancyTime += 1
          if(inPregnancyTime != 0 && inPregnancyTime % energyRequirementsIncreasePeriod == 0)
            publish(PregnancyRequirements(energyRequirementsPerChild * embryos.size / energyRequirementsIncreaseSteps))
          if(inPregnancyTime >= pregnancyDurationInClocks) {
            val sons = embryos
            embryos = Seq()
            inPregnancyTime = 0
            publish(PregnancyEnd())
            requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest).onComplete({
              case Success(info) =>
                publish(NewMutantAlleles(sons.flatMap(s=>GeneticsSimulator.checkNewMutation(s.species.name,s.genome)).toSeq))
                val newEntities = sons.map(i => animalCreationFunction(i, info.position))
                publish(Create(newEntities))
              case Failure(exception) =>
                throw exception
            })
          }
        }
        publish(new ComputeNextStateAck)
      case InteractionEntity(partnerId, kind) => kind match {
        case Couple =>
          checkPartnerExistence(partnerId, partnerBaseInfo => {
            obtainPersonalData((myBaseInfo, myPhysicalInfo) => {
              import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
              if (partnerBaseInfo.state.head.state.status == EntityUpdateState.UPDATED) {
                publish(AutoForceReproduction(partnerId))
                publish(PartnerForceReproduction(partnerId, animalGenome, myPhysicalInfo.fertility, partnerBaseInfo.state.head.state.species.toString))
              } else if(embryos.isEmpty) {
                publish(PartnerForceReproduction(partnerId, animalGenome, myPhysicalInfo.fertility, partnerBaseInfo.state.head.state.species.toString))
                copulate(partnerId, myBaseInfo.gender, myBaseInfo.species, partnerBaseInfo.state.head.state.species.toString,
                  myPhysicalInfo.fertility)
              }
            })
          })
        case _ =>
      }
      case r: PartnerInfoRequest =>
        requireData[ReproductionPhysicalInformationRequest, ReproductionPhysicalInformationResponse](ReproductionPhysicalInformationRequest())
          .onComplete{
            case Success(info) =>
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
          copulate(partnerId, myBaseInfo.gender, myBaseInfo.species, partnerBaseInfo.state.head.state.species.toString,
            myPhysicalInfo.fertility)
        })
      })
    case r: PartnerForceReproduction =>
      obtainPersonalData((myBaseInfo, myPhysicalInfo) => {
        copulate(r, myBaseInfo.gender, myBaseInfo.species, myPhysicalInfo.fertility)
      })
  }

  private def checkPartnerExistence(partnerId: String, f: EntitiesStateResponse => Unit): Unit = {
    requireData[EntitiesStateRequest, EntitiesStateResponse](core.EntitiesStateRequest(x => x.entityId == partnerId))
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

  private def copulate(partnerId: String, gender: String, species: String, partnerSpecies: String, myFertility: Double): Unit = {
    if(GenderTypes.withNameOpt(gender).get == GenderTypes.female) {
      requireData[PartnerInfoRequest, PartnerInfoResponse](PartnerInfoRequest(partnerId, entitySpecifications.id))
      .onComplete {
        case Success(partner) =>
          createEmbryos(ReproductionInfo(animalGenome, myFertility, species),
            ReproductionInfo(partner.partnerGenome, partner.partnerFertility, partnerSpecies))
        case Failure(error) => throw error
      }
    }
  }

  private def copulate(partner: ReproductionInfo, gender: String, species: String, myFertility: Double): Unit = {
    if (GenderTypes.withNameOpt(gender).get == GenderTypes.female) {
      createEmbryos(ReproductionInfo(animalGenome, myFertility, species), partner)
    }
  }

  private def createEmbryos(me: ReproductionInfo, partner: ReproductionInfo): Unit = {
    embryos = EmbryosUtil.createEmbryos(me, partner, fecundity)
    if(embryos.nonEmpty) {
      publish(Pregnant())
    }
  }
}

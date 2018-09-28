package it.unibo.pps.ese.model.components.animals.reproduction

import it.unibo.pps.ese.controller.simulation.runner.core
import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.{BaseEvent, InteractionEvent, RequestEvent, ResponseEvent}
import it.unibo.pps.ese.model.components.animals.reproduction.util.{EmbryosUtil, GeneticsEngine, ReproductionInfo}
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.support._
import it.unibo.pps.ese.model.components._
import it.unibo.pps.ese.model.components.animals.brain.{Couple, InteractionEntity}
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.GenderTypes
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.dna.{AnimalGenome, MGene}
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo
import it.unibo.pps.ese.utils.Point

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/** Message that notifies new mutant allele spread*/
case class NewMutantAlleles(mutantGenes:Seq[MGene]) extends BaseEvent

/** Message to request animal's base info*/
case class ReproductionBaseInformationRequest() extends RequestEvent

/** Response to [[it.unibo.pps.ese.model.components.animals.reproduction.ReproductionBaseInformationRequest]]
  *
  * @param id Request id
  * @param gender Animal's gender
  * @param species Aniaml's species
  */
case class ReproductionBaseInformationResponse(override val id: String, gender: String, species: String) extends ResponseEvent

/** Message to request animal's base physical info*/
case class ReproductionPhysicalInformationRequest() extends RequestEvent

/**Response to [[it.unibo.pps.ese.model.components.animals.reproduction.ReproductionPhysicalInformationRequest]]
  *
  * @param id Request id
  * @param fertility Animal's current fertility
  */
case class ReproductionPhysicalInformationResponse(override val id: String, fertility: Double) extends ResponseEvent

/** Message to request to another animal his reproduction related info
  *
  * @param receiverId Request receiver id
  * @param senderId Request sender id
  */
case class PartnerInfoRequest(override val receiverId: String, senderId: String) extends InteractionEvent with RequestEvent

/** Response to [[it.unibo.pps.ese.model.components.animals.reproduction.PartnerInfoRequest]]
  *
  * @param id Request id
  * @param receiverId Receiver id
  * @param partnerGenome Partner genome
  * @param partnerFertility Partner fertility
  */
case class PartnerInfoResponse(override val id: String, override val receiverId: String, partnerGenome: AnimalGenome,
                               partnerFertility: Double) extends InteractionEvent with ResponseEvent

/** Message that notifies pregnancy start*/
case class Pregnant() extends BaseEvent

/** Message that notifies an extra energy requirement caused by fetus growing
  *
  * @param extraEnergyRequirements extra energy requirements
  */
case class PregnancyRequirements(extraEnergyRequirements: Double) extends BaseEvent

/** Message that notifies pregnancy end*/
case class PregnancyEnd() extends BaseEvent

/*
 * Messages for sync partners' copulation
 */

/** Trait that defines a message class that force reproduction as next animal's action*/
trait ForceReproduction

/** Message to force me to copulate next era
  *
  * @param partnerId Copulation partner's id
  */
case class AutoForceReproduction(partnerId: String) extends ForceReproduction with BaseEvent

/** Message to force partner to copulate as next action. Also contains reproduction information for partner
  *
  * @param receiverId Receiver id
  * @param genome Animal's genome
  * @param fertility Animal's fertility
  * @param species Animal's species
  */
case class PartnerForceReproduction(override val receiverId: String, genome: AnimalGenome, fertility: Double, species: String)
  extends ForceReproduction with InteractionEvent with ReproductionInfo

/** Message to forward [[it.unibo.pps.ese.model.components.animals.reproduction.ForceReproduction]] messages to ReproductionComponent
  *
  * @param forwardedForcing Original ForceReproduction message
  * @tparam T Original ForceReproduction message type
  */
case class ForceReproductionForward[T <: ForceReproduction](forwardedForcing: T) extends BaseEvent

/** Animal's component that models reproduction and pregnancy. Its functions include copulation synchronization between
  * partners and alleles mutation
  *
  * @param entitySpecifications Belonging entity's info
  * @param _fecundity Animal's fecundity
  * @param _geneticsSimulator Genetics simulator for non-personal genetics services
  * @param _animalGenome Animal's genome
  * @param _pregnancyDuration Animal's pregnancy duration
  * @param _clocksPerYear Year duration in clocks
  * @param _mutationProb Mutant alleles spread probability
  * @param _energyRequirements Animal initial energy requirements
  * @param _animalCreationFunction Function that can create [[it.unibo.pps.ese.controller.simulation.runner.core.Entity]]
  *                                starting from AnimalInfo and position
  * @param executionContext Execution context
  */
class ReproductionComponent(override val entitySpecifications: EntitySpecifications,
                            _fecundity: Double,
                            _geneticsSimulator: GeneticsSimulator,
                            _animalGenome: AnimalGenome,
                            _pregnancyDuration: Double,
                            _clocksPerYear: Long,
                            _mutationProb: Double,
                            _energyRequirements: Double,
                            _animalCreationFunction: (AnimalInfo, Point) => Entity)
                                (implicit val executionContext: ExecutionContext)
                                  extends WriterComponent(entitySpecifications)  {

  private val energyRequirementsPerChild = _energyRequirements * 0.2 / math.round(_fecundity)
  private val energyRequirementsIncreaseSteps = 2
  private val pregnancyDurationInClocks: Long = Seq((_clocksPerYear * _pregnancyDuration).toLong, energyRequirementsIncreaseSteps + 1).max
  private val energyRequirementsIncreasePeriod = pregnancyDurationInClocks / (energyRequirementsIncreaseSteps + 1)
  private var embryos: Seq[AnimalInfo] = Seq()
  private var inPregnancyTime: Long = 0

  implicit val geneticEngine: GeneticsEngine = GeneticsEngine(_geneticsSimulator, _mutationProb)

  override def initialize(): Unit = {
    subscribeEvents()
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        if(embryos.nonEmpty) {
          inPregnancyTime += 1
          if(inPregnancyTime >= pregnancyDurationInClocks) {
            val sons = embryos
            embryos = Seq()
            inPregnancyTime = 0
            publish(PregnancyEnd())
            requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest).onComplete({
              case Success(info) =>
                publish(NewMutantAlleles(sons.flatMap(s=>GeneticsSimulator.checkNewMutation(s.species.name,s.genome)).toSeq))
                val newEntities = sons.map(i => _animalCreationFunction(i, info.position))
                publish(Create(newEntities))
              case Failure(exception) =>
                throw exception
            })
          } else if (inPregnancyTime != 0 && inPregnancyTime % energyRequirementsIncreasePeriod == 0) {
            publish(PregnancyRequirements(energyRequirementsPerChild * embryos.size / energyRequirementsIncreaseSteps))
          }
        }
        publish(new ComputeNextStateAck)
      case InteractionEntity(partnerId, kind) => kind match {
        case Couple =>
          checkPartnerExistence(partnerId, partnerBaseInfo => {
            obtainPersonalData((myBaseInfo, myPhysicalInfo) => {
              import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
              if (partnerBaseInfo.state.head.state.status == EntityUpdateState.UPDATED) {
                /*
                 * If partner has already moved in this era, reproduction is scheduled for next era for both
                 */
                publish(AutoForceReproduction(partnerId))
                publish(PartnerForceReproduction(partnerId, _animalGenome, myPhysicalInfo.fertility, partnerBaseInfo.state.head.state.species.toString))
              } else if(embryos.isEmpty) {
                /*
                 * If partner has not already moved in this era, and i'm not already pregnant, reproduction happens in this era
                 */
                publish(PartnerForceReproduction(partnerId, _animalGenome, myPhysicalInfo.fertility, partnerBaseInfo.state.head.state.species.toString))
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
              publish(PartnerInfoResponse(r.id, r.senderId, _animalGenome, info.fertility))
            case Failure(exception) =>
              throw exception
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

  /** Tries to obtain partner base info represented by [[it.unibo.pps.ese.controller.simulation.runner.core.EntitiesStateResponse]].
    * If info retrieval is successful (partner exists), info are consumed by given function
    *
    * @param partnerId Partner id
    * @param f Callback function
    */
  private def checkPartnerExistence(partnerId: String, f: EntitiesStateResponse => Unit): Unit = {
    requireData[EntitiesStateRequest, EntitiesStateResponse](core.EntitiesStateRequest(x => x.entityId == partnerId))
      .onComplete{
        case Success(value) =>
          if(value.state.nonEmpty) {
            f(value)
          }
        case Failure(exception) =>
          throw exception
      }
  }

  /** Retrieves personal base information, represented by [[it.unibo.pps.ese.model.components.animals.reproduction.ReproductionBaseInformationResponse]]
    * and [[it.unibo.pps.ese.model.components.animals.reproduction.ReproductionPhysicalInformationResponse]] and pass them
    * to given callback function
    *
    * @param f Callback function
    */
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

  /** Make copulation (embryos creation) with a partner of witch i don't know all reproduction's info
    *
    * @param partnerId Partner's id
    * @param gender Animal's gender
    * @param species Animal's species
    * @param partnerSpecies Partner's gender
    * @param myFertility Animal's fertility
    */
  private def copulate(partnerId: String, gender: String, species: String, partnerSpecies: String, myFertility: Double): Unit = {
    if(GenderTypes.withNameOpt(gender).get == GenderTypes.female) {
      requireData[PartnerInfoRequest, PartnerInfoResponse](PartnerInfoRequest(partnerId, entitySpecifications.id))
      .onComplete {
        case Success(partner) =>
          createEmbryos(ReproductionInfo(_animalGenome, myFertility, species),
            ReproductionInfo(partner.partnerGenome, partner.partnerFertility, partnerSpecies))
        case Failure(error) => throw error
      }
    }
  }

  /** Make copulation (embryos creation) with a partner with known reproduction's info
    *
    * @param partner Partner's reproduction's info
    * @param gender Animal's gender
    * @param species Animal's specie
    * @param myFertility Animal's fertility
    */
  private def copulate(partner: ReproductionInfo, gender: String, species: String, myFertility: Double): Unit = {
    if (GenderTypes.withNameOpt(gender).get == GenderTypes.female) {
      createEmbryos(ReproductionInfo(_animalGenome, myFertility, species), partner)
    }
  }

  /** Create embryos starting from animal's reproduction's info*/
  private def createEmbryos(me: ReproductionInfo, partner: ReproductionInfo): Unit = {
    embryos = EmbryosUtil.createEmbryos(me, partner, _fecundity)
    if(embryos.nonEmpty) {
      publish(Pregnant())
    }
  }
}

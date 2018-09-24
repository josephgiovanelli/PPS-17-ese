package it.unibo.pps.ese.model.components.animals

import it.unibo.pps.ese.controller.simulation.runner.core
import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.{BaseEvent, InteractionEvent}
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.model.components.animals.LifePhases.LifePhases
import it.unibo.pps.ese.model.components.animals.brain.{DynamicParametersRequest, DynamicParametersResponse, Eat, InteractionEntity}
import it.unibo.pps.ese.model.components.animals.reproduction.{PregnancyEnd, PregnancyRequirements, ReproductionPhysicalInformationRequest, ReproductionPhysicalInformationResponse}

import scala.concurrent.ExecutionContext
import scala.math.floor
import scala.util.{Failure, Success}

object LifePhases extends Enumeration {
  type LifePhases = Value
  val CHILD, ADULT, ELDERLY = Value
}

case class DigestionEnd() extends BaseEvent
case class MealInformation(override val receiverId: String, eatenEnergy: Double) extends InteractionEvent
case class PhysicalStatusInfo(averageLife: Double,
                               energyRequirements: Double,
                               endChildPhase: Double,
                               endAdultPhase: Double,
                               percentageDecay: Double,
                               speed: Double,
                               fertility: Double) extends BaseEvent

case class DynamicPhysicalStatusInfo(age: Int,
                                     energy: Double,
                                     lifePhase: LifePhases,
                                     actualSpeed: Double,
                                     actualFertility: Double) extends BaseEvent

case class PhysicalStatusComponent(override val entitySpecifications: EntitySpecifications,
                                   averageLife: Double,
                                   energyRequirements: Double,
                                   endChildPhase: Double,
                                   endAdultPhase: Double,
                                   percentageDecay: Double,
                                   speed: Double,
                                   fertility: Double,
                                   yearToClock: Long)
                                  (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications)  {

  val MAX_ENERGY = 1000
  val MAX_SATISFACTION = 100
  val MIN_DIGESTION_TIME = 10
  val MAX_DIGESTION_TIME = 40

  var currentYear: Int = 0
  var currentEnergy: Double = MAX_ENERGY
  var currentPhase: LifePhases = LifePhases.CHILD
  var currentSpeed: Double = speed
  var currentFertility: Double = 0
  var elapsedClocksSinceLastYear: Int = 0
  var elapsedClocksSinceDigestion: Int = 0
  var digestionTime: Int = 0
  var extraEnergyRequirements: Double = 0
  var satisfaction: Double = 0

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def dynamicInfo: DynamicPhysicalStatusInfo = this synchronized {
    DynamicPhysicalStatusInfo(currentYear, currentEnergy, currentPhase, currentSpeed, currentFertility)
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        this synchronized {
          if (satisfaction != 0) {
            satisfaction -= 1
          }
          if (digestionTime == 0) {
            currentEnergy -= (energyRequirements + extraEnergyRequirements)
          }
          else {
            if (elapsedClocksSinceDigestion < digestionTime) {
              elapsedClocksSinceDigestion += 1
            }
            else {
              digestionTime = 0
              elapsedClocksSinceDigestion = 0
              publish(DigestionEnd())
            }
          }
        }
        publish(dynamicInfo)
        if (currentEnergy <= 0) publish(Kill(entitySpecifications id))
        elapsedClocksSinceLastYear += 1
        if (elapsedClocksSinceLastYear == yearToClock) yearCallback()
        publish(new ComputeNextStateAck)
      case PregnancyRequirements(value) =>
        extraEnergyRequirements += value
      case _: PregnancyEnd =>
        extraEnergyRequirements = 0
      case r: DynamicParametersRequest =>
        publish(DynamicParametersResponse(r.id, currentSpeed, currentEnergy, currentFertility, satisfaction))
      case r: ReproductionPhysicalInformationRequest =>
        publish(ReproductionPhysicalInformationResponse(r id, currentFertility))
      case InteractionEntity(entityId, action) => action match {
        case Eat => requireData[EntitiesStateRequest, EntitiesStateResponse](core.EntitiesStateRequest(x => x.entityId == entityId))
          .onComplete {
            case Success(result) =>
              import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
              val necessaryEnergy = MAX_ENERGY - currentEnergy
              val eatenEnergy = if (result.state.head.state.nutritionalValue > necessaryEnergy)
                necessaryEnergy else result.state.head.state.nutritionalValue
              this synchronized {
                currentEnergy += eatenEnergy
              }
              digestionTime = (((eatenEnergy / MAX_ENERGY) * MAX_DIGESTION_TIME) + MIN_DIGESTION_TIME).toInt
              elapsedClocksSinceDigestion = 0
              publish(dynamicInfo)
              publish(MealInformation(entityId, eatenEnergy))
            case Failure(error) => throw error
          }
        case _ => satisfaction = MAX_SATISFACTION
      }
      case MealInformation(_, _) =>
        publish(Kill(entitySpecifications id))
      case GetInfo() =>
        publish(dynamicInfo)
        publish(PhysicalStatusInfo(averageLife, energyRequirements, endChildPhase, endAdultPhase, percentageDecay, speed, currentFertility))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  private def configureMappings(): Unit = {
    addMapping[PhysicalStatusInfo]((classOf[PhysicalStatusInfo], ev => Seq(
      EntityProperty("averageLife", ev averageLife),
      EntityProperty("energyRequirements", ev energyRequirements),
      EntityProperty("endChildPhase", ev endChildPhase),
      EntityProperty("endAdultPhase", ev endAdultPhase),
      EntityProperty("percentageDecay", ev percentageDecay),
      EntityProperty("speed", ev speed),
      EntityProperty("fertility", ev fertility)
    )))
    addMapping[DynamicPhysicalStatusInfo]((classOf[DynamicPhysicalStatusInfo], ev => Seq(
      EntityProperty("age", ev age),
      EntityProperty("energy", ev energy),
      EntityProperty("lifePhase", ev lifePhase),
      EntityProperty("actualSpeed", ev actualSpeed),
      EntityProperty("actualFertility", ev actualFertility)
    )))
  }

  private def yearCallback(): Unit = this synchronized {
    elapsedClocksSinceLastYear = 0
    currentYear += 1
    val myThreshold: Double = currentYear * percentageDecay

    currentPhase match {
      case LifePhases.CHILD if myThreshold > endChildPhase =>
        currentPhase = LifePhases.ADULT
        currentFertility = fertility
      case LifePhases.ADULT if myThreshold > endAdultPhase =>
        currentPhase = LifePhases.ELDERLY
        currentFertility = 0
      case LifePhases.ELDERLY =>
        currentSpeed = speed - (currentSpeed * percentageDecay)
      case _ =>
    }

    if (currentYear == floor(averageLife * percentageDecay)) publish(Kill(entitySpecifications id))
    publish(dynamicInfo)
  }
}

package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.support.BaseEvent

import scala.math.floor
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

case class Dead() extends BaseEvent
case class PhysicalStatusInfo(averageLife: Double,
                               energyRequirements: Double,
                               nutritiveValue: Double,
                               endChildPhase: Double,
                               endAdultPhase: Double,
                               percentageDecay: Double,
                               speed: Double,
                               fertility: Double
                             ) extends BaseEvent

case class PhysicalStatusComponent(override val entitySpecifications: EntitySpecifications,
                                   averageLife: Double,
                                   energyRequirements: Double,
                                   nutritiveValue: Double,
                                   endChildPhase: Double,
                                   endAdultPhase: Double,
                                   percentageDecay: Double,
                                   speed: Double,
                                   fertility: Double
                                  ) extends WriterComponent(entitySpecifications)  {

  val MAX_ENERGY = 100
  val YEAR_TO_CLOCK = 10

  object LifePhases extends Enumeration {
    val CHILD, ADULT, ELDERLY = Value
  }

  var currentYear: Int = 0
  var currentEnergy: Double = MAX_ENERGY
  var currentPhase: LifePhases.Value = LifePhases.CHILD
  var currentSpeed: Double = speed
  var elapsedClocks: Int = 0

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        currentEnergy -= energyRequirements
        if (currentEnergy <= 0) publish(Dead())
        elapsedClocks += 1
        if (elapsedClocks == YEAR_TO_CLOCK) yearCallback()
        publish(new ComputeNextStateAck)
      case r: DynamicParametersRequest =>
        publish(DynamicParametersResponse(r.id, speed, currentEnergy, fertility))
      case EatEntity(entityId) =>
        requireData[EntitiesStateRequest, EntitiesStateResponse](EntitiesStateRequest(x => x.entityId == entityId))
          .onComplete {
            case Success(result) =>
              import EntityInfoConversion._
              currentEnergy += result.state.head.state.nutritiveValue
              if(currentEnergy > MAX_ENERGY) currentEnergy = MAX_ENERGY
            case Failure(error) => throw error
          }
      case GetInfo() =>
        publish(PhysicalStatusInfo(averageLife, energyRequirements, nutritiveValue, endChildPhase, endAdultPhase, percentageDecay, speed, fertility))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  private def configureMappings(): Unit = {
    addMapping[PhysicalStatusInfo]((classOf[PhysicalStatusInfo], ev => Seq(
      EntityProperty("averageLife", ev averageLife),
      EntityProperty("energyRequirements", ev energyRequirements),
      EntityProperty("nutritiveValue", ev nutritiveValue),
      EntityProperty("endChildPhase", ev endChildPhase),
      EntityProperty("endAdultPhase", ev endAdultPhase),
      EntityProperty("percentageDecay", ev percentageDecay),
      EntityProperty("speed", ev speed),
      EntityProperty("fertility", ev fertility)
    )))
  }

  private def yearCallback(): Unit = {
    elapsedClocks = 0
    currentYear += 1
    if (currentPhase == LifePhases.CHILD && currentYear > endChildPhase) currentPhase = LifePhases.ADULT
    else if (currentPhase == LifePhases.ADULT && currentYear > endAdultPhase) currentPhase = LifePhases.ELDERLY
    else if (currentPhase == LifePhases.ELDERLY) currentSpeed = speed * percentageDecay
    if (currentYear == floor(averageLife * percentageDecay)) publish(Dead())
  }
}

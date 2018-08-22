package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.support.BaseEvent

import scala.math.floor

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
        publish(new ComputeNextStateResponse)
      case RequireDynamicParameters() =>
        publish(RequireDynamicParametersResponse(speed, currentEnergy, fertility))
      case EatEntity(entityId) =>
        publish(RequireEntitiesState(entitySpecifications id, x => x.entityId == entityId))
      case EntitiesStateResponse(id, states) if id == entitySpecifications.id =>
        import EntityInfoConversion._
        currentEnergy += states.head.state.nutritiveValue
        if(currentEnergy > MAX_ENERGY) currentEnergy = MAX_ENERGY
      case GetInfo() =>
        publish(PhysicalStatusInfo(averageLife, energyRequirements, nutritiveValue, endChildPhase, endAdultPhase, percentageDecay, speed, fertility))
        publish(new GetInfoResponse)
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

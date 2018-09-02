package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.support.{BaseEvent, InteractionEvent}

import scala.math.floor
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

case class MealInformation(override val receiverId: String, eatenEnergy: Double) extends InteractionEvent
case class PhysicalStatusInfo(averageLife: Double,
                               energyRequirements: Double,
                               endChildPhase: Double,
                               endAdultPhase: Double,
                               percentageDecay: Double,
                               speed: Double,
                               fertility: Double
                             ) extends BaseEvent

case class PhysicalStatusComponent(override val entitySpecifications: EntitySpecifications,
                                   averageLife: Double,
                                   energyRequirements: Double,
                                   endChildPhase: Double,
                                   endAdultPhase: Double,
                                   percentageDecay: Double,
                                   speed: Double,
                                   fertility: Double
                                  ) extends WriterComponent(entitySpecifications)  {

  val MAX_ENERGY = 10000
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
        if (currentEnergy <= 0) publish(Kill(entitySpecifications id))
        elapsedClocks += 1
        if (elapsedClocks == YEAR_TO_CLOCK) yearCallback()
        publish(new ComputeNextStateAck)
      case r: DynamicParametersRequest =>
        publish(DynamicParametersResponse(r.id, currentSpeed, currentEnergy, fertility))
      case r: ReproductionPhysicalInformationRequest =>
        publish(ReproductionPhysicalInformationResponse(r id, fertility, currentYear))
      case InteractionEntity(entityId, action) if action == ActionKind.EAT =>
        requireData[EntitiesStateRequest, EntitiesStateResponse](EntitiesStateRequest(x => x.entityId == entityId))
          .onComplete {
            case Success(result) =>
              import EntityInfoConversion._
              val necessaryEnergy = MAX_ENERGY - currentEnergy
              val eatenEnergy = if (result.state.head.state.nutritionalValue > necessaryEnergy)
                necessaryEnergy else result.state.head.state.nutritionalValue
              currentEnergy += eatenEnergy
              publish(MealInformation(entityId, eatenEnergy))
              //println("Tasty! (Prey : " + entityId + ", Energy : " + eatenEnergy +  ", Predator : " + entitySpecifications.id + ")")
            case Failure(error) => throw error
          }
      case MealInformation(targetId, _) if entitySpecifications.id == targetId =>
        //println("OMG!!1!!1! I've been killed! (Id : " + entitySpecifications.id +")")
        publish(Kill(entitySpecifications id))
      case GetInfo() =>
        publish(PhysicalStatusInfo(averageLife, energyRequirements, endChildPhase, endAdultPhase, percentageDecay, speed, fertility))
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
  }

  private def yearCallback(): Unit = {
    elapsedClocks = 0
    currentYear += 1
    if (currentPhase == LifePhases.CHILD && currentYear > endChildPhase) currentPhase = LifePhases.ADULT
    else if (currentPhase == LifePhases.ADULT && currentYear > endAdultPhase) currentPhase = LifePhases.ELDERLY
    else if (currentPhase == LifePhases.ELDERLY) currentSpeed = speed * percentageDecay
    if (currentYear == floor(averageLife * percentageDecay)) publish(Kill(entitySpecifications id))
  }
}

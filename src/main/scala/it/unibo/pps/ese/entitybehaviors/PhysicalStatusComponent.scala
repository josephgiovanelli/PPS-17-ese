package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.support.{BaseEvent, InteractionEvent}

import scala.concurrent.ExecutionContext
import scala.math.floor
import scala.util.{Failure, Success}

object LifePhases extends Enumeration {
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
                                     lifePhase: LifePhases.Value,
                                     actualSpeed: Double) extends BaseEvent

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
  val MIN_DIGESTION_TIME = 10
  val MAX_DIGESTION_TIME = 40

  var currentYear: Int = 0
  var currentEnergy: Double = MAX_ENERGY
  var currentPhase: LifePhases.Value = LifePhases.CHILD
  var currentSpeed: Double = speed
  var elapsedClocksSinceLastYear: Int = 0
  var elapsedClocksSinceDigestion: Int = 0
  var digestionTime: Int = 0
  var extraEnergyRequirements: Double = 0

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def dynamicInfo: DynamicPhysicalStatusInfo = this synchronized {
    DynamicPhysicalStatusInfo(currentYear, currentEnergy, currentPhase, currentSpeed)
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        this synchronized {
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
        publish(DynamicParametersResponse(r.id, currentSpeed, currentEnergy, fertility))
      case r: ReproductionPhysicalInformationRequest =>
        publish(ReproductionPhysicalInformationResponse(r id, fertility))
      case InteractionEntity(entityId, action) if action == ActionKind.EAT =>
        requireData[EntitiesStateRequest, EntitiesStateResponse](EntitiesStateRequest(x => x.entityId == entityId))
          .onComplete {
            case Success(result) =>
              import EntityInfoConversion._
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
              //println("Tasty! (Prey : " + entityId + ", Energy : " + eatenEnergy +  ", Predator : " + entitySpecifications.id + ")")
            case Failure(error) => throw error
          }
      case MealInformation(_, _) =>
        //println("OMG!!1!!1! I've been killed! (Id : " + entitySpecifications.id +")")
        //publish(Kill(entitySpecifications id))
      case GetInfo() =>
        publish(dynamicInfo)
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
    addMapping[DynamicPhysicalStatusInfo]((classOf[DynamicPhysicalStatusInfo], ev => Seq(
      EntityProperty("age", ev age),
      EntityProperty("energy", ev energy),
      EntityProperty("lifePhase", ev lifePhase),
      EntityProperty("actualSpeed", ev actualSpeed)
    )))
  }

  private def yearCallback(): Unit = this synchronized {
    elapsedClocksSinceLastYear = 0
    currentYear += 1
    if (currentPhase == LifePhases.CHILD && currentYear > endChildPhase) currentPhase = LifePhases.ADULT
    else if (currentPhase == LifePhases.ADULT && currentYear > endAdultPhase) currentPhase = LifePhases.ELDERLY
    else if (currentPhase == LifePhases.ELDERLY) currentSpeed = speed * percentageDecay
    if (currentYear == floor(averageLife * percentageDecay)) publish(Kill(entitySpecifications id))
    publish(dynamicInfo)
  }

  override def serialize: AbstractComponentMemento = ???
}

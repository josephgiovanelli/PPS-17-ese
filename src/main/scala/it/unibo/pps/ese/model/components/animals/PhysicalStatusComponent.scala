package it.unibo.pps.ese.model.components.animals

import it.unibo.pps.ese.controller.simulation.runner.core
import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.{BaseEvent, InteractionEvent}
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.model.components.animals.LifePhases.LifePhases
import it.unibo.pps.ese.model.components.animals.brain._
import it.unibo.pps.ese.model.components.animals.reproduction.{PregnancyEnd, PregnancyRequirements, ReproductionPhysicalInformationRequest, ReproductionPhysicalInformationResponse}

import scala.concurrent.ExecutionContext
import scala.math.floor
import scala.util.{Failure, Success}

/**
  * Enumeration that defines the life phases that the entity can assume.
  */
object LifePhases extends Enumeration {
  type LifePhases = Value
  val CHILD, ADULT, ELDERLY = Value
}

/**
  * It communicates that the digestion phase ends.
  */
case class DigestionEnd() extends BaseEvent

/**
  * It communicates that the receiverId was eaten.
  * @param receiverId the prey
  * @param eatenEnergy the energy taken from the prey
  */
case class MealInformation(override val receiverId: String, eatenEnergy: Double) extends InteractionEvent

/**
  * Event with which inform the rest of the world of [[PhysicalStatusComponent]] state.
  * @param averageLife the average life of the entity
  * @param energyRequirements the energy requirements of the entity
  * @param endChildPhase the age at the end of the child phase of the entity
  * @param endAdultPhase the age at the end of the adult phase of the entity
  * @param percentageDecay the percentage decay of the entity
  * @param speed the initial speed of the entity
  * @param fertility the initial fertility of the entity
  */
case class PhysicalStatusInfo(averageLife: Double,
                               energyRequirements: Double,
                               endChildPhase: Double,
                               endAdultPhase: Double,
                               percentageDecay: Double,
                               speed: Double,
                               fertility: Double) extends BaseEvent

/**
  * Event with which inform the rest of the world of the dynamic parameters.
  * @param age the current age of the entity
  * @param energy the current energy of the entity
  * @param lifePhase the current life phase of the entity
  * @param currentSpeed the current speed of the entity
  * @param currentFertility the current fertility of the entity
  */
case class DynamicPhysicalStatusInfo(age: Int,
                                     energy: Double,
                                     lifePhase: LifePhases,
                                     currentSpeed: Double,
                                     currentFertility: Double) extends BaseEvent

/**
  * The component that is responsible for the current status of the entity, which includes all the physiological characteristics
  * @param entitySpecifications the base information of the entity
  * @param averageLife the average life of the entity
  * @param energyRequirements the energy requirements of the entity
  * @param endChildPhase the age at the end of the child phase of the entity
  * @param endAdultPhase the age at the end of the adult phase of the entity
  * @param percentageDecay the percentage decay of the entity
  * @param speed the initial speed of the entity
  * @param fertility the initial fertility of the entity
  * @param yearToClock the conversion of the years in terms of clock
  * @param executionContext the execution context
  */
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

  /**
    * Bounds of dynamic parameters.
    */
  val MAX_ENERGY = 1000
  val MAX_SATISFACTION = 100
  val MIN_DIGESTION_TIME = 10
  val MAX_DIGESTION_TIME = 40

  /**
    * The current state of dynamic parameters.
    */
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

  /**
    * It creates the message in order to communicate the dynamic parameters
    * @return the message in subject
    */
  private def dynamicInfo: DynamicPhysicalStatusInfo = this synchronized {
    DynamicPhysicalStatusInfo(currentYear, currentEnergy, currentPhase, currentSpeed, currentFertility)
  }

  /**
    * Method by which events are received and the brain specifies the reaction to them.
    */
  private def subscribeEvents(): Unit = {
    subscribe {
      /*
      At each clock the internal status is updated.
       */
      case ComputeNextState() =>
        this synchronized {
          //sexual satisfaction decreases
          if (satisfaction != 0) {
            satisfaction -= 1
          }
          //if you are not in digestion, energy is reduced
          if (digestionTime == 0) {
            currentEnergy -= (energyRequirements + extraEnergyRequirements)
          }
          //otherwise no and continue until the digestion ends
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
        //if the energy is 0, you are dead
        if (currentEnergy <= 0) publish(Kill(entitySpecifications id))
        //the clock is increased
        elapsedClocksSinceLastYear += 1
        //if a year has passed, the physical conditions will be further updated
        if (elapsedClocksSinceLastYear == yearToClock) yearCallback()
        publish(new ComputeNextStateAck)
      /*
      The embryo we have in our body has grown which needs more energy.
       */
      case PregnancyRequirements(value) =>
        extraEnergyRequirements += value
      /*
      The pregnancy period is over.
       */
      case _: PregnancyEnd =>
        extraEnergyRequirements = 0
      /*
      Request of dynamic parameters by another component.
       */
      case r: DynamicParametersRequest =>
        publish(DynamicParametersResponse(r.id, currentSpeed, currentEnergy, currentFertility, satisfaction))
      /*
      Request of fertility parameter by reproduction component.
       */
      case r: ReproductionPhysicalInformationRequest =>
        publish(ReproductionPhysicalInformationResponse(r id, currentFertility))
      /*
      An action has been taken and it is reacted appropriately.
      */
      case InteractionEntity(entityId, action) => action match {
        /*
        In case of a eat, it increases the energy of the eaten quantity and proceeds to begin the digestion phase.
        */
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
        /*
        In case of a couple, it increases the satisfaction.
        */
        case Couple => satisfaction = MAX_SATISFACTION
        case _ => Unit
      }
      /*
      It is communicated that the entity has been killed.
       */
      case MealInformation(_, _) =>
        publish(Kill(entitySpecifications id))
      /*
      If this message is received the physical status has to communicate his information.
       */
      case GetInfo() =>
        publish(dynamicInfo)
        publish(PhysicalStatusInfo(averageLife, energyRequirements, endChildPhase, endAdultPhase, percentageDecay, speed, currentFertility))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  /**
    * The exterior can see the component as a set of attributes.
    * These are modified by the published events.
    * This method defines how events change attributes.
    */
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
      EntityProperty("actualSpeed", ev currentSpeed),
      EntityProperty("actualFertility", ev currentFertility)
    )))
  }

  /**
    * Every entity, according to the phase of life, every year that passes, degrades and its characteristics are affected.
    * Each entity has a percentage of decay that affects the fall.
    */
  private def yearCallback(): Unit = this synchronized {
    elapsedClocksSinceLastYear = 0
    currentYear += 1

    /*
    An entity to become fertile must overcome puberty and stop being so when old age begins.
    An entity when it becomes elderly begins to degrade.
     */
    currentPhase match {
      case LifePhases.CHILD if currentYear > (endChildPhase * percentageDecay) =>
        currentPhase = LifePhases.ADULT
        currentFertility = fertility
      case LifePhases.ADULT if currentYear > (endAdultPhase * percentageDecay)  =>
        currentPhase = LifePhases.ELDERLY
        currentFertility = 0
      case LifePhases.ELDERLY =>
        currentSpeed = speed - (currentSpeed * percentageDecay)
      case _ =>
    }

    //Based on the percentage of decay an individual could die before or after the average life of that species.
    if (currentYear == floor(averageLife * percentageDecay)) publish(Kill(entitySpecifications id))
    publish(dynamicInfo)
  }
}

package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.entitybehaviors.MealInformation
import it.unibo.pps.ese.genericworld.model.support.BaseEvent
import it.unibo.pps.ese.utils.Point

case class PlantPhysicalInfo(position: Point, height: Double, nutritionalValue: Double, availability: Double,
                             hardness: Double,
                             gender: String) extends BaseEvent

case class PlantPhysicalComponent(override val entitySpecifications: EntitySpecifications,
                             position: Point,
                             height: Double,
                             var nutritionalValue: Double,
                             availability: Double,
                             hardness: Double,
                             gender:String) extends WriterComponent(entitySpecifications) {

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case MealInformation(targetId, eatenEnergy) if targetId == entitySpecifications.id =>
      this synchronized {
        //println("They are eating me! (Id : " + entitySpecifications.id + ", Energy : " + eatenEnergy + ")")
        nutritionalValue = nutritionalValue - eatenEnergy
        if (nutritionalValue <= 0) {
          publish(Kill(entitySpecifications id))
          //println("I've been completely eaten! (Id : " + entitySpecifications.id +")")
        }
      }
    case ComputeNextState() =>
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      publish(PlantPhysicalInfo(position, height, nutritionalValue, availability, hardness, gender))
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[PlantPhysicalInfo]((classOf[PlantPhysicalInfo], ev => Seq(
      EntityProperty("position", ev position),
      EntityProperty("height", ev height),
      EntityProperty("nutritiveValue", ev nutritionalValue),
      EntityProperty("availability", ev availability),
      EntityProperty("defense", ev hardness),
      EntityProperty("gender", ev gender)
    )))
  }
}

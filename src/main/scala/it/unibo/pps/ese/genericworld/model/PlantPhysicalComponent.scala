package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.entitybehaviors.MealInformation
import it.unibo.pps.ese.genericworld.model.support.BaseEvent
import it.unibo.pps.ese.utils.Point

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

case class EntityNutritionalValue(nutritionalValue: Double) extends BaseEvent
case class PlantPhysicalInfo(availability: Double) extends BaseEvent

case class PlantPhysicalComponent(override val entitySpecifications: EntitySpecifications,
                                  availability: Double)
                                 (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications) {

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case MealInformation(targetId, eatenEnergy) if targetId == entitySpecifications.id =>
      val result = requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest)
      result onComplete {
        case Success(data) =>
          //println("They are eating me! (Id : " + entitySpecifications.id + ", Energy : " + eatenEnergy + ")")
          val updatedNutritionalValue = data.nutritionalValue - eatenEnergy
          publish(EntityNutritionalValue(updatedNutritionalValue))
          if (updatedNutritionalValue <= 0) {
            publish(Kill(entitySpecifications id))
            //println("I've been completely eaten! (Id : " + entitySpecifications.id +")")
          }
        case Failure(error) => throw error
      }
    case ComputeNextState() =>
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      publish(PlantPhysicalInfo(availability))
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[PlantPhysicalInfo]((classOf[PlantPhysicalInfo], ev => Seq(
      EntityProperty("availability", ev availability)
    )))
  }
}

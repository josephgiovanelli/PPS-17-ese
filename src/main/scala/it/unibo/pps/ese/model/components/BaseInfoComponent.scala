package it.unibo.pps.ese.model.components

import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.support.{RequestEvent, ResponseEvent}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.components.animals.brain.EntityPosition
import it.unibo.pps.ese.model.components.animals.reproduction.{ReproductionBaseInformationRequest, ReproductionBaseInformationResponse}
import it.unibo.pps.ese.model.components.plant.EntityNutritionalValue
import it.unibo.pps.ese.utils.Point

import scala.concurrent.ExecutionContext

case class BaseInfoRequest() extends RequestEvent
case class BaseInfoResponse(override val id: String,
                            species: String,
                            reign: ReignType.Value,
                            position: Point,
                            height: Double,
                            nutritionalValue: Double,
                            defense: Double,
                            gender:String,
                            entityInfo: it.unibo.pps.ese.model.genetics.entities.EntityInfo) extends ResponseEvent

case class BaseInfoComponent(override val entitySpecifications: EntitySpecifications,
                             species: String,
                             reign: ReignType.Value,
                             gender:String,
                             var position: Point,
                             height: Double,
                             var nutritionalValue: Double,
                             defense: Double,
                             entityInfo: it.unibo.pps.ese.model.genetics.entities.EntityInfo)
                            (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications) {

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case EntityPosition(newPosition) =>
      this synchronized {
        position = newPosition
      }
    case EntityNutritionalValue(newNutritionalValue) =>
      this synchronized {
        nutritionalValue = newNutritionalValue
      }
    case r: BaseInfoRequest =>
      this synchronized {
        publish(BaseInfoResponse(r id, species, reign, position, height, nutritionalValue, defense, gender, entityInfo))
      }
    case r: ReproductionBaseInformationRequest =>
        publish(ReproductionBaseInformationResponse(r id, gender, species))
    case ComputeNextState() =>
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      this synchronized {
        publish(BaseInfoResponse("", species, reign, position, height, nutritionalValue, defense, gender, entityInfo))
      }
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[BaseInfoResponse]((classOf[BaseInfoResponse], ev => Seq(
      EntityProperty("species", ev species),
      EntityProperty("reign", ev reign),
      EntityProperty("position", ev position),
      EntityProperty("height", ev height),
      EntityProperty("nutritionalValue", ev nutritionalValue),
      EntityProperty("defense", ev defense),
      EntityProperty("gender", ev gender),
      EntityProperty("entityInfo", ev entityInfo)
    )))
  }
}

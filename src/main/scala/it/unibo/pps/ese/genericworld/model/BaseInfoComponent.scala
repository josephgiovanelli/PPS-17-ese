package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.entitybehaviors.EntityPosition
import it.unibo.pps.ese.genericworld.model.support.{RequestEvent, ResponseEvent}
import it.unibo.pps.ese.utils.Point

case class BaseInfoRequest() extends RequestEvent
case class BaseInfoResponse(override val id: String,
                            species: String,
                            reign: ReignType.Value,
                            position: Point,
                            height: Double,
                            nutritionalValue: Double,
                            defense: Double,
                            gender:String) extends ResponseEvent(id)

case class BaseInfoComponent(override val entitySpecifications: EntitySpecifications,
                        species: String,
                        reign: ReignType.Value,
                        gender:String,
                        var position: Point,
                        height: Double,
                        var nutritionalValue: Double,
                        defense: Double) extends WriterComponent(entitySpecifications) {

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
      publish(BaseInfoResponse(r id, species, reign, position, height, nutritionalValue, defense, gender))
    case ComputeNextState() =>
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      publish(BaseInfoResponse("", species, reign, position, height, nutritionalValue, defense, gender))
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
      EntityProperty("gender", ev gender)
    )))
  }
}

package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support.BaseEvent

case class BaseInfo(species: String, reign: String) extends BaseEvent

case class BaseInfoComponent(override val entitySpecifications: EntitySpecifications,
                        species: String,
                        reign: String) extends WriterComponent(entitySpecifications) {

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case ComputeNextState() =>
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      publish(BaseInfo(species, reign))
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[BaseInfo]((classOf[BaseInfo], ev => Seq(
      EntityProperty("species", ev species),
      EntityProperty("reign", ev reign)
    )))
  }
}

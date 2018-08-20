package it.unibo.pps.ese.genericWorld.model

import it.unibo.pps.ese.genericWorld.model.support.BaseEvent
import scala.language.dynamics

case class ExampleEvent(speed: Int) extends BaseEvent

class ExampleComponent(override val entitySpecifications: EntitySpecifications) extends WriterComponent(entitySpecifications) {

  private var speed = 0

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case EntitiesStateResponse(id, state) if id == "example" =>
      val myState : Option[EntityInfo] = state find (s => s.entityId == entitySpecifications.id) map (s => s state)
      if (myState isDefined) {
        import EntityInfoConversion._
        val a : Int = (myState get).speed
        println("speed : " + a)
      }
    case ComputeNextState() =>
      speed += 1
      publish(ExampleEvent(speed))
      publish(new ComputeNextStateResponse)
    case GetInfo() =>
      publish(ExampleEvent(speed))
      publish(new GetInfoResponse)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[ExampleEvent]((classOf[ExampleEvent], ev => Seq(EntityProperty("speed", ev speed))))
  }
}

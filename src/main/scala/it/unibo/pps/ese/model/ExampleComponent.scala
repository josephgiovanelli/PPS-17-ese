package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.BaseEvent
import scala.language.dynamics

case class ExampleEvent(speed: Int) extends BaseEvent

class ExampleComponent(override val entityId: String) extends WriterComponent(entityId) {

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case ev: ExampleEvent => {
      println(ev speed)
      publish(RequireEntitiesState("example"))
    }
    case EntitiesStateResponse(id, state) if id == "example" => {
      println("response : " + state)
      val myState : Option[EntityInfo] = state find (s => s.entityId == entityId) map (s => s state)
      if (myState isDefined) {
        val a = (myState get).speed
        println("speed : " + a)
      }

    }
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[ExampleEvent]((classOf[ExampleEvent], ev => Seq(EntityProperty("speed", 1l, ev speed))))
  }
}

package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.BaseEvent
import scala.language.dynamics

case class ExampleEvent(speed: Int) extends BaseEvent

class ExampleComponent(override val entitySpecifications: EntitySpecifications) extends WriterComponent(entitySpecifications) {

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = subscribe {
    case ev: ExampleEvent =>
      println(ev speed)
      publish(RequireEntitiesState("example"))
    case EntitiesStateResponse(id, state) if id == "example" =>
      println("response : " + state)
      val myState : Option[EntityInfo] = state find (s => s.entityId == entitySpecifications.id) map (s => s state)
      if (myState isDefined) {
        import EntityInfoConversion._
        val a : Int = (myState get).speed
        println("speed : " + a)
      }
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[ExampleEvent]((classOf[ExampleEvent], ev => Seq(EntityProperty("speed", ev speed))))
  }
}

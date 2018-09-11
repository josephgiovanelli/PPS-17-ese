package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support.BaseEvent

import scala.concurrent.ExecutionContext
import scala.language.dynamics

case class ExampleEvent(speed: Int) extends BaseEvent

class ExampleComponent(override val entitySpecifications: EntitySpecifications)
                      (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications) {

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
        val a : Double = (myState get).fakeSpeed
        println("speed : " + a)
      }
    case ComputeNextState() =>
      speed += 1
      publish(ExampleEvent(speed))
      publish(new ComputeNextStateAck)
    case GetInfo() =>
      publish(ExampleEvent(speed))
      publish(new GetInfoAck)
    case _ => Unit
  }

  private def configureMappings(): Unit = {
    addMapping[ExampleEvent]((classOf[ExampleEvent], ev => Seq(EntityProperty("fakeSpeed", ev speed))))
  }

  override def serialize: AbstractComponentMemento = ???
}

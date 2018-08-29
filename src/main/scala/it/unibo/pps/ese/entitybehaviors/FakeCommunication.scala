package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.FakeEvent.FakeEvent
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldTypesImpl

object FakeEventType extends Enumeration {
  val DEAD, CALCULATE_NEXT_MOVE, NEXT_MOVE, EAT_ENTITY, NEXT_YEAR = Value
  val NAME, HEIGHT, STRONG, DEFENSE, KIND, SPEED, VISUAL_FIELD, ACTION_FIELD, AVERAGE_LIFE, ENERGY_REQUIREMENTS,
    NUTRITIVE_VALUE, END_CHILD_PHASE, END_ADULT_PHASE, PERCENTAGE_DECAY = Value
}


object FakeEvent {
  def apply(eventType: FakeEventType.Value, body: String): FakeEvent = FakeEvent(eventType, body)
  def unapply(arg: FakeEvent): Option[(FakeEventType.Value, String)] = Some((arg.event, arg.body))

  case class FakeEvent(event: FakeEventType.Value, body: String)
}

abstract class FakeComponent(fakeBus: FakeBus) {
  fakeBus.register(this)

  def consume(fakeEvent: FakeEvent)
}

class FakeBus {
  var fakeComponents: Set[FakeComponent] = Set.empty

  def register(fakeComponent: FakeComponent): Unit = fakeComponents += fakeComponent
  def publish(fakeEvent: FakeEvent): Unit = fakeComponents foreach (fakeComponent => fakeComponent.consume(fakeEvent))
}

case class FakePoint(x: Int, y: Int)
case class FakeEntityRepresentation(name: Int, kind: String, height: Int, strong: Int, defense: Int, point: FakePoint, nutritiveValue: Int)

sealed trait FakeBuffer extends WorldTypesImpl {
  def getEntityInVisualField(id: Int): Set[FakeEntityRepresentation]
  def setEntityInVisualField(id: Int, entities: Set[FakeEntityRepresentation]): Unit
}

object FakeBuffer {
  private val _instance = new FakeBufferImpl()
  def instance() =
    _instance

  class FakeBufferImpl() extends FakeBuffer {
    private var buffer: Map[Int, Set[FakeEntityRepresentation]] = Map.empty

    override def getEntityInVisualField(id: Int): Set[FakeEntityRepresentation] = if (buffer.get(id).isDefined) buffer(id) else Set.empty
    override def setEntityInVisualField(id: Int, entities: Set[FakeEntityRepresentation]): Unit = buffer += (id -> entities)
  }
}




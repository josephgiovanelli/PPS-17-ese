package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.FakeEvent.FakeEvent

object FakeEventType extends Enumeration {
  val PING, PONG, HEIGHT, STRONG, DEFENSE, KIND, SPEED, VISUAL_FIELD, ACTION_FIELD, AVERAGE_LIFE, ENERGY_REQUIREMENTS,
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


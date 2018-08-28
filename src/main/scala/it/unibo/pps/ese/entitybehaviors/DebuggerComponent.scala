package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.FakeEvent.FakeEvent

case class DebuggerComponent(fakeBus: FakeBus) extends FakeComponent(fakeBus: FakeBus) {

  override def consume(fakeEvent: FakeEvent): Unit = fakeEvent match {
    case FakeEvent(a, b) if a.equals(FakeEventType.NEXT_MOVE) => println("spostato " + b)
    case FakeEvent(a, b) if a.equals(FakeEventType.EAT_ENTITY) => println("mangiato " + b)
    case _ => Unit
  }

}

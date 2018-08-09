package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.FakeEvent.FakeEvent

case class PhysicalStatusComponent(fakeBus: FakeBus) extends FakeComponent(fakeBus: FakeBus) {

  var currentYear: Int = 0
  var currentEnergy: Int = 100


  override def consume(fakeEvent: FakeEvent): Unit = fakeEvent match {
    case FakeEvent(a, b) if a.equals(FakeEventType.PONG) => {
      println("ping")
      fakeBus.publish(FakeEvent(FakeEventType.PING, (b.toInt + 1).toString))
    }
    case _ => Unit
  }

}

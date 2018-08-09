package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.FakeEvent._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.DecisionSupport


case class BrainComponent(fakeBus: FakeBus) extends FakeComponent(fakeBus: FakeBus) {

  val decisionSupport: DecisionSupport = DecisionSupport()

  override def consume(fakeEvent: FakeEvent): Unit = fakeEvent match {
    case FakeEvent(a, b) if a.equals(FakeEventType.PING) => {
      println("pong")
    }
    case _ => Unit
  }
}

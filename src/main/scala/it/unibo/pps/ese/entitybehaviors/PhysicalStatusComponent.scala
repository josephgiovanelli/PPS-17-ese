package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.FakeEvent.FakeEvent

case class PhysicalStatusComponent(fakeBus: FakeBus) extends FakeComponent(fakeBus: FakeBus) {

  var currentYear: Int = 0
  var currentEnergy: Int = 100

  var averageLife: Option[Int] = None
  var energyRequirements: Option[Int] = None
  var nutritiveValue: Option[Int] = None
  var endChildPhase: Option[Int] = None
  var endAdultPhase: Option[Int] = None
  var percentageDecay: Option[Float] = None

  var speed: Option[Int] = None
  var visualField: Option[Int] = None
  var actionField: Option[Int] = None


  override def consume(fakeEvent: FakeEvent): Unit = fakeEvent match {
    case FakeEvent(a, b) if a.equals(FakeEventType.PONG) => println("ping"); fakeBus.publish(FakeEvent(FakeEventType.PING, (b.toInt + 1).toString))
    case FakeEvent(a, b) if a.equals(FakeEventType.AVERAGE_LIFE) => averageLife = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.ENERGY_REQUIREMENTS) => energyRequirements = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.NUTRITIVE_VALUE) => nutritiveValue = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.END_CHILD_PHASE) => endChildPhase = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.END_ADULT_PHASE) => endAdultPhase = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.PERCENTAGE_DECAY) => percentageDecay = Some(b.toFloat)
    case FakeEvent(a, b) if a.equals(FakeEventType.SPEED) => speed = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.VISUAL_FIELD) => visualField = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.ACTION_FIELD) => actionField = Some(b.toInt)

    case FakeEvent(a, b) if a.equals(FakeEventType.NEXT_MOVE) => println(b)
    case _ => Unit
  }

}

package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.FakeEvent.FakeEvent
import scala.math.floor

case class PhysicalStatusComponent(fakeBus: FakeBus, speed: Int) extends FakeComponent(fakeBus: FakeBus) {

  val MAX_ENERGY = 100

  object LifePhases extends Enumeration {
    val CHILD, ADULT, ELDERLY = Value
  }

  var currentYear: Int = 0
  var currentEnergy: Int = MAX_ENERGY
  var currentPhase: LifePhases.Value = LifePhases.CHILD

  var name: Option[Int] = None
  var averageLife: Option[Int] = None
  var energyRequirements: Option[Int] = None
  var nutritiveValue: Option[Int] = None
  var endChildPhase: Option[Int] = None
  var endAdultPhase: Option[Int] = None
  var percentageDecay: Option[Float] = None

  var currentSpeed: Int = speed
  fakeBus.publish(FakeEvent(FakeEventType.SPEED, speed.toString))



  override def consume(fakeEvent: FakeEvent): Unit = fakeEvent match {
    case FakeEvent(a, b) if a.equals(FakeEventType.NAME) => name = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.AVERAGE_LIFE) => averageLife = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.ENERGY_REQUIREMENTS) => energyRequirements = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.NUTRITIVE_VALUE) => nutritiveValue = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.END_CHILD_PHASE) => endChildPhase = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.END_ADULT_PHASE) => endAdultPhase = Some(b.toInt)
    case FakeEvent(a, b) if a.equals(FakeEventType.PERCENTAGE_DECAY) => percentageDecay = Some(b.toFloat)

    case FakeEvent(a, b) if a.equals(FakeEventType.NEXT_MOVE) => {
      currentEnergy -= energyRequirements.get
      if (currentEnergy <= 0) fakeBus.publish(FakeEvent(FakeEventType.DEAD, ""))
    }
    case FakeEvent(a, b) if a.equals(FakeEventType.NEXT_YEAR) => {
      currentYear += 1
      if (currentPhase == LifePhases.CHILD && currentYear > endChildPhase.get) currentPhase = LifePhases.ADULT
      else if (currentPhase == LifePhases.ADULT && currentYear > endAdultPhase.get) currentPhase = LifePhases.ELDERLY
      else if (currentPhase == LifePhases.ELDERLY) {
        currentSpeed = floor(speed * percentageDecay.get).toInt
        fakeBus.publish(FakeEvent(FakeEventType.SPEED, currentSpeed.toString))
      }
      if (currentYear == floor(averageLife.get * percentageDecay.get)) fakeBus.publish(FakeEvent(FakeEventType.DEAD, ""))
    }
    case FakeEvent(a, b) if a.equals(FakeEventType.EAT_ENTITY) => {
      val fakeEntityRepresentation: FakeEntityRepresentation =FakeBuffer.instance().getEntityInVisualField(name.get) filter (x => x.name.equals(b.toInt)) head;
      currentEnergy += fakeEntityRepresentation.nutritiveValue
      if(currentEnergy > MAX_ENERGY) currentEnergy = MAX_ENERGY
    }
    case _ => Unit
  }

}

package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.StaticRules
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._

object TryCommunication extends App {

  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
  val worldRules: WorldRulesImpl = decisionsupport.WorldRulesImpl.WorldRulesImpl(3, (0, 5), Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
    Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))

  StaticRules.instance().setRules(worldRules)
  val fakeBus: FakeBus = new FakeBus

  val brain: BrainComponent = BrainComponent(fakeBus)
  val physicalStatus: PhysicalStatusComponent = PhysicalStatusComponent(fakeBus)

  fakeBus.publish(FakeEvent(FakeEventType.PONG, "0"))

  //deve acquisirli Brain
  fakeBus.publish(FakeEvent(FakeEventType.HEIGHT, "6"))
  fakeBus.publish(FakeEvent(FakeEventType.STRONG, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.DEFENSE, "5"))
  fakeBus.publish(FakeEvent(FakeEventType.KIND, "carnivorous"))

  //devono acquisirli Brain e PhysicalStatus
  fakeBus.publish(FakeEvent(FakeEventType.SPEED, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.VISUAL_FIELD, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.ACTION_FIELD, "5"))

  //deve acquisirli PhysicalStatus
  fakeBus.publish(FakeEvent(FakeEventType.AVERAGE_LIFE, "50"))
  fakeBus.publish(FakeEvent(FakeEventType.ENERGY_REQUIREMENTS, "4"))
  fakeBus.publish(FakeEvent(FakeEventType.NUTRITIVE_VALUE, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.END_CHILD_PHASE, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.END_ADULT_PHASE, "30"))
  fakeBus.publish(FakeEvent(FakeEventType.PERCENTAGE_DECAY, "0.3"))

}

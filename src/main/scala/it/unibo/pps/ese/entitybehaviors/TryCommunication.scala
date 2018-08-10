package it.unibo.pps.ese.entitybehaviors

import javax.tools.Diagnostic.Kind

import apple.laf.JRSUIState.TitleBarHeightState
import it.unibo.pps.ese.entitybehaviors.decisionsupport.StaticRules
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._

object TryCommunication extends App {

  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
  val worldRules: WorldRulesImpl = decisionsupport.WorldRulesImpl.WorldRulesImpl(3, (0, 5), Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
    Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))

  StaticRules.instance().setRules(worldRules)
  val fakeBus: FakeBus = new FakeBus

  val brain: BrainComponent = BrainComponent(fakeBus, FakePoint(3, 3))
  val physicalStatus: PhysicalStatusComponent = PhysicalStatusComponent(fakeBus, 2)
  val debuggerStatus: DebuggerComponent = DebuggerComponent(fakeBus)


  //deve acquisirli Brain
  fakeBus.publish(FakeEvent(FakeEventType.NAME, "4"))
  fakeBus.publish(FakeEvent(FakeEventType.HEIGHT, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.STRONG, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.DEFENSE, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.KIND, "carnivorous"))

  //devono acquisirli Brain e PhysicalStatus
  fakeBus.publish(FakeEvent(FakeEventType.VISUAL_FIELD, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.ACTION_FIELD, "5"))

  //deve acquisirli PhysicalStatus
  fakeBus.publish(FakeEvent(FakeEventType.AVERAGE_LIFE, "50"))
  fakeBus.publish(FakeEvent(FakeEventType.ENERGY_REQUIREMENTS, "4"))
  fakeBus.publish(FakeEvent(FakeEventType.NUTRITIVE_VALUE, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.END_CHILD_PHASE, "10"))
  fakeBus.publish(FakeEvent(FakeEventType.END_ADULT_PHASE, "30"))
  fakeBus.publish(FakeEvent(FakeEventType.PERCENTAGE_DECAY, "0.3"))

  val prey0 = FakeEntityRepresentation(0, "plant", 5, 2, 2, FakePoint(5, 6), 3)
  val prey1 = FakeEntityRepresentation(1, "herbivore", 6, 6, 6, FakePoint(7, 6), 4)
  val prey2 = FakeEntityRepresentation(2, "herbivore", 7, 7, 7, FakePoint(2, 1), 5)
  val prey3 = FakeEntityRepresentation(3, "herbivore", 6, 6, 6, FakePoint(8, 1), 6)
  val prey5 = FakeEntityRepresentation(5, "carnivorous", 9, 9, 9, FakePoint(4, 3), 7)

  FakeBuffer.instance().setEntityInVisualField(4, Set(prey0, prey1, prey2, prey3, prey5))

  fakeBus.publish(FakeEvent(FakeEventType.CALCULATE_NEXT_MOVE, ""))
  fakeBus.publish(FakeEvent(FakeEventType.CALCULATE_NEXT_MOVE, ""))
  fakeBus.publish(FakeEvent(FakeEventType.CALCULATE_NEXT_MOVE, ""))



}
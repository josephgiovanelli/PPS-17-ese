package it.unibo.pps.ese.entitybehaviors.decisionsupport.prologimplementation

import it.unibo.pps.ese.StaticRules
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.{DecisionSupport, EntityChoiceImpl, EntityKinds, GeneralPosition}
import org.scalatest.FunSuite

class PrologDecisionSupportTest extends FunSuite {

  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))

  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
  val worldRules: WorldRulesImpl = WorldRulesImpl(3, (0, 5), Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
    Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))
  StaticRules.instance().setRules(worldRules)

  val prey1 = EntityAttributesImpl(1, EntityKinds('herbivore), 6, 6, 6, (6, 6))
  val prey2 = EntityAttributesImpl(2, EntityKinds('herbivore), 6, 6, 6, (2, 1))
  val hunter = EntityAttributesImpl(3, EntityKinds('carnivorous), 10, 10, 10, (3, 3))
  val decisionSupport: DecisionSupport = PrologDecisionSupport()
  decisionSupport.createVisualField(Seq(prey2, hunter))
  val firstTest: Stream[EntityChoiceImpl] = decisionSupport.discoverPreys(hunter)

  test("Hunter with a prey has a not empty result.") {
    assert(firstTest.lengthCompare(1) == 0)
  }

  val newHunterPosition: GeneralPosition[decisionSupport.PositionMeasure] = decisionSupport.nextMove(hunter, prey2)

  test("The new calculated position must be closer than the original.") {
    val originalDistance = (hunter.position.x - prey2.position.x) + (hunter.position.y - prey2.position.y)
    val newDistance = (newHunterPosition.x - prey2.position.x) + (newHunterPosition.y - prey2.position.y)
    assert(newDistance < originalDistance)
  }

  val decisionSupport2: DecisionSupport = PrologDecisionSupport()
  decisionSupport2.createVisualField(Seq(prey1, prey2, hunter))
  decisionSupport2.clearVisualField()
  val thirdTest: Stream[EntityChoiceImpl] = decisionSupport2.discoverPreys(hunter)

  test("clearVisualField must delete all entities.") {
    assert(thirdTest.lengthCompare(0) == 0)
  }


}

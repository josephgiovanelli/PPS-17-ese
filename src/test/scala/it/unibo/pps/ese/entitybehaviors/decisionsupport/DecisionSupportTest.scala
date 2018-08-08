package it.unibo.pps.ese.entitybehaviors.decisionsupport

import it.unibo.pps.ese.entitybehaviors.StaticRules
import it.unibo.pps.ese.entitybehaviors.WorldRules._
import org.scalatest.FunSuite

class DecisionSupportTest extends FunSuite {

  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))

  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
  val worldRules: WorldRules = WorldRules(3, (0, 5), Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
    Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))
  StaticRules.instance().setRules(worldRules)

  val prey1 = EntityAttributes(1, EntityKinds('herbivore), 6, 6, 6, (6, 6))
  val prey2 = EntityAttributes(2, EntityKinds('herbivore), 6, 6, 6, (2, 1))
  val hunter = EntityAttributes(3, EntityKinds('carnivorous), 10, 10, 10, (3, 3))

  val decisionSupport: DecisionSupport = DecisionSupport()
  decisionSupport.createVisualField(Seq(prey2, hunter))
  val firstTest: Stream[EntityChoice] = decisionSupport.discoverPreys(hunter)

  test("Hunter with a prey has a not empty result.") {
    assert(firstTest.lengthCompare(1) == 0)
  }

  val newHunterPosition: (Int, Int) = decisionSupport.nextMove(hunter, prey2)

  test("The new calculated position must be closer than the original.") {
    val originalDistance = (hunter.position._1 - prey2.position._1) + (hunter.position._2 - prey2.position._2)
    val newDistance = (newHunterPosition._1 - prey2.position._1) + (newHunterPosition._2 - prey2.position._2)
    assert(newDistance < originalDistance)
  }

  val decisionSupport2: DecisionSupport = DecisionSupport()
  decisionSupport2.createVisualField(Seq(prey1, prey2, hunter))
  decisionSupport2.clearVisualField()
  val thirdTest: Stream[EntityChoice] = decisionSupport2.discoverPreys(hunter)

  test("clearVisualField must delete all entities.") {
    assert(thirdTest.lengthCompare(0) == 0)
  }
}

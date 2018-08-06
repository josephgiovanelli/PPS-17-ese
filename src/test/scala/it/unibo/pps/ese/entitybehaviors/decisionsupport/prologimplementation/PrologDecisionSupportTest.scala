package it.unibo.pps.ese.entitybehaviors.decisionsupport.prologimplementation

import alice.tuprolog.Term
import it.unibo.pps.ese.entitybehaviors.decisionsupport.{DecisionSupport, EntityAttributes, EntityKinds}
import org.scalatest.FunSuite

class PrologDecisionSupportTest extends FunSuite {

  val prey1 = EntityAttributes(1, EntityKinds.herbivore, 6, 6, 6, (6, 6))
  val prey2 = EntityAttributes(2, EntityKinds.herbivore, 6, 6, 6, (2, 1))
  val hunter = EntityAttributes(3, EntityKinds.carnivorous, 10, 10, 10, (3, 3))

  val decisionSupport: DecisionSupport = PrologDecisionSupport()
  decisionSupport.createVisualField(Seq(prey2, hunter))
  val firstTest: Stream[(Int, Int)] = decisionSupport.discoverPreys(hunter)

  test("Hunter with a prey has a not empty result.") {
    assert(firstTest.lengthCompare(1) == 0)
  }

  val newHunterPosition: (Int, Int) = decisionSupport.nextMove(hunter, prey2)

  test("The new calculated position must be closer than the original.") {
    val originalDistance = scala.math.pow(hunter.position._1 - prey2.position._1, 2) + scala.math.pow(hunter.position._2 - prey2.position._2, 2)
    val newDistance = scala.math.pow(newHunterPosition._1 - prey2.position._1, 2) + scala.math.pow(newHunterPosition._2 - prey2.position._2, 2)
    assert(newDistance < originalDistance)
  }

  val decisionSupport2: DecisionSupport = PrologDecisionSupport()
  decisionSupport2.createVisualField(Seq(prey1, prey2, hunter))
  decisionSupport2.clearVisualField()
  val thirdTest: Stream[(Int, Int)] = decisionSupport2.discoverPreys(hunter)

  test("clearVisualField must delete all entities.") {
    assert(thirdTest.lengthCompare(0) == 0)
  }


}

package it.unibo.pps.ese.entitybehaviors.decisionsupport.prologimplementation

import it.unibo.pps.ese.entitybehaviors.StaticRules
import it.unibo.pps.ese.entitybehaviors.WorldRules._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.{DecisionSupport, EntityAttributes, EntityKinds}

object PerformanceTest extends App {

  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
  val worldRules: WorldRules = WorldRules(3, (0, 5), Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
    Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))

  StaticRules.instance().setRules(worldRules)

  val prey0 = EntityAttributes(0, EntityKinds('plant), 5, 2, 2, (5, 6))
  val prey1 = EntityAttributes(1, EntityKinds('herbivore), 6, 6, 6, (6, 6))
  val prey2 = EntityAttributes(2, EntityKinds('herbivore), 7, 7, 7, (2, 1))
  val prey3 = EntityAttributes(3, EntityKinds('herbivore), 6, 6, 6, (3, 1))
  val prey4 = EntityAttributes(4, EntityKinds('carnivorous), 10, 10, 10, (3, 3))
  val prey5 = EntityAttributes(5, EntityKinds('carnivorous), 9, 9, 9, (4, 3))
  val decisionSupport: DecisionSupport = PrologDecisionSupport()

  val startTime = System.nanoTime

  //for(_ <- 1 to 100) {
    decisionSupport.createVisualField(Seq(prey1, prey2, prey3, prey4, prey5))
    decisionSupport.discoverPreys(prey4) foreach (println(_))
    decisionSupport.clearVisualField()
  //}

  val estimatedTime = System.nanoTime - startTime
  println(estimatedTime)
}


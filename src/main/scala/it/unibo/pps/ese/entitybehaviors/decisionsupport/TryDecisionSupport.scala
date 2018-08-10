package it.unibo.pps.ese.entitybehaviors.decisionsupport

import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl._



object TryDecisionSupport extends App {

  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
  val worldRules: WorldRulesImpl = WorldRulesImpl(3, (0, 5), Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
    Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))

  StaticRules.instance().setRules(worldRules)


  val prey0 = EntityAttributesImpl(0, EntityKinds('plant), 5, 2, 2, (5, 6))
  val prey1 = EntityAttributesImpl(1, EntityKinds('herbivore), 6, 6, 6, (6, 6))
  val prey2 = EntityAttributesImpl(2, EntityKinds('herbivore), 7, 7, 7, (2, 1))
  val prey3 = EntityAttributesImpl(3, EntityKinds('herbivore), 6, 6, 6, (3, 1))
  val prey4 = EntityAttributesImpl(4, EntityKinds('carnivorous), 10, 10, 10, (3, 3))
  val prey5 = EntityAttributesImpl(5, EntityKinds('carnivorous), 9, 9, 9, (4, 3))


  val decisionSupport: DecisionSupport = DecisionSupport()

  val startTime = System.nanoTime

  //for(_ <- 1 to 100) {
    decisionSupport.createVisualField(Seq(prey1, prey2, prey3, prey4, prey5))
    decisionSupport.discoverPreys(prey4) foreach (println(_))
    decisionSupport.clearVisualField()
  //}

  val estimatedTime = System.nanoTime - startTime
  println(estimatedTime)


  /*val prova = Prova(MyEnum('v1))
  MyEnum.values.foreach(println)
  println(prova.myEnum match {
    case MyEnum('v1) => "ciao"
    case _ => "ehi"
  })*/

}

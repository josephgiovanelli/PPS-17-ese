package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityKinds.EntityKinds
import it.unibo.pps.ese.entitybehaviors.decisionsupport.{EntityKinds, WorldTypesImpl}
import it.unibo.pps.ese.genetics.GeneticsSimulator

sealed trait StaticRules extends WorldTypesImpl {
  def getSpecies(): Set[String]
  def addSpecies(kinds: Set[String]): Unit

  def updateRules(): Unit

  def setRules(rules: WorldRules): Unit
  def getRules(): WorldRules
}


object StaticRules {
  private val _instance = new StaticRulesImpl()
  def instance() =
    _instance

  class StaticRulesImpl() extends StaticRules {
    private var entityKinds: Set[String] = Set.empty
    private var worldRules: Option[WorldRules] = None

    override def getSpecies(): Set[String] = entityKinds
    override def addSpecies(kinds: Set[String]): Unit = entityKinds ++= kinds

    override def updateRules(): Unit = {
      val geneticsSimulator = GeneticsSimulator
      entityKinds = (geneticsSimulator.plantSpeciesList ++ geneticsSimulator.speciesList).toSet
      EntityKinds.updateSpecies()
      val carnivorous: Set[String] = geneticsSimulator.simulationData.get.animals.keySet.filter(entity => entity.typology == "C").map(entity => entity.name)
      val herbivore: Set[String] = geneticsSimulator.simulationData.get.animals.keySet.filter(entity => entity.typology == "H").map(entity => entity.name)
      val plants: Seq[String] = geneticsSimulator.plantSpeciesList
      var huntingRules: Seq[(String, String)] = Seq.empty
      carnivorous.foreach(c => herbivore.foreach(h => huntingRules = huntingRules :+ (c, h)))
      herbivore.foreach(h => plants.foreach(p => huntingRules = huntingRules :+ (h, p)))
      worldRules.get.setCompatibleHuntingKinds(huntingRules.toSet)
      worldRules.get.setCompatibleCouplingKinds((carnivorous ++ herbivore).map(species => (species, species)))
    }

    override def setRules(rules: WorldRules): Unit = {
      worldRules = Some(rules)
      updateRules()
    }
    override def getRules(): WorldRules = worldRules.get
  }
}




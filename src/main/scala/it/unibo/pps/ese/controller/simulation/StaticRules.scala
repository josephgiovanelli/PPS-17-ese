package it.unibo.pps.ese.controller.simulation

import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.{EntityKinds, WorldTypesImpl}
import it.unibo.pps.ese.model.genetics.GeneticsSimulator

/**
  * Trait through which the rules of the world can be accessed and changed.
  */
sealed trait StaticRules extends WorldTypesImpl {

  /**
    * The method that allows knowledge of species in the world.
    * @return a set that contains the species
    */
  def getSpecies: Set[String]

  /**
    * Add species in the world.
    * @param kinds the set of species to add
    */
  def addSpecies(kinds: Set[String]): Unit

  /**
    * The method that allows to set the rules of the world.
    * @param rules the rules to set
    */
  def setRules(rules: WorldRules): Unit

  /**
    * The method that allows knowledge of rules of bthe world
    * @return the rules of the world
    */
  def getRules: WorldRules

  /**
    * On the basis of all the species set up, if the species are present also in the [[GeneticsSimulator]], it generates the relations between them.
    */
  def updateRules(): Unit
}

/**
  * Singleton Implementation of [[StaticRules]] that allows to access to services at any time.
  */
object StaticRules {
  private val _instance = new StaticRulesImpl()
  def instance(): StaticRulesImpl = _instance

  class StaticRulesImpl() extends StaticRules {
    private var entityKinds: Set[String] = Set.empty
    private var worldRules: Option[WorldRules] = None

    override def getSpecies: Set[String] = entityKinds

    override def addSpecies(kinds: Set[String]): Unit = entityKinds ++= kinds

    override def setRules(rules: WorldRules): Unit = {
      worldRules = Some(rules)
      if(worldRules.get.compatibleCouplingKinds.isEmpty || worldRules.get.compatibleHuntingKinds.isEmpty) {
        updateRules()
      }
    }

    override def getRules: WorldRules = worldRules.getOrElse(throw new IllegalStateException())

    override def updateRules(): Unit = {
      val geneticsSimulator = GeneticsSimulator
      entityKinds = (geneticsSimulator.plantSpeciesList ++ geneticsSimulator.speciesList).toSet
      EntityKinds.updateSpecies()
      val carnivorous: Set[String] = geneticsSimulator.simulationData.get.animals.keySet.filter(entity => entity.typology == "C").map(entity => entity.name)
      val herbivore: Set[String] = geneticsSimulator.simulationData.get.animals.keySet.filter(entity => entity.typology == "H").map(entity => entity.name)
      val plants: Seq[String] = geneticsSimulator.plantSpeciesList
      worldRules.get.setCompatibleHuntingKinds(carnivorous.zip(herbivore) ++ herbivore.zip(plants))
      worldRules.get.setCompatibleCouplingKinds((carnivorous ++ herbivore).map(species => (species, species)))
    }

  }
}




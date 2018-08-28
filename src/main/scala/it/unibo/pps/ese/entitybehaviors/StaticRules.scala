package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldTypesImpl

sealed trait StaticRules extends WorldTypesImpl {
  def getSpecies(): Set[String]
  def addSpecies(kinds: Set[String]): Unit

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

    override def setRules(rules: WorldRules): Unit = worldRules = Some(rules)
    override def getRules(): WorldRules = worldRules.get
  }
}




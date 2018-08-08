package it.unibo.pps.ese.entitybehaviors

import it.unibo.pps.ese.entitybehaviors.WorldRules.WorldRules
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityKinds

sealed trait StaticRules {
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

object WorldRules {
  def apply(attackThreshold: Int, heightThresholds: (Int, Int), compatibleHuntingKinds: Set[(EntityKinds.Value, EntityKinds.Value)], compatibleCouplingKinds: Set[(EntityKinds.Value, EntityKinds.Value)]): WorldRules =  WorldRules(attackThreshold, heightThresholds, compatibleHuntingKinds, compatibleCouplingKinds)
  implicit def stringToEntityKinds(string: String): EntityKinds.Value = EntityKinds(Symbol(string))
  implicit def tupleStringToEntityKinds(tuple: (String, String)): (EntityKinds.Value, EntityKinds.Value) = (tuple._1, tuple._2)
  implicit def setTupleStringToSetTupleEntityKinds(set: Set[(String, String)]): Set[(EntityKinds.Value, EntityKinds.Value)] = set map tupleStringToEntityKinds

  case class WorldRules(attackThreshold: Int, heightThresholds: (Int, Int), compatibleHuntingKinds: Set[(EntityKinds.Value, EntityKinds.Value)], compatibleCouplingKinds: Set[(EntityKinds.Value, EntityKinds.Value)])
}


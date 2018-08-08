package it.unibo.pps.ese.entitybehaviors

sealed trait StaticRules {
  def getValues(): Set[String]
  def addValues(kinds: Set[String]): Unit
  def prova(): Unit
}


object StaticRules {
  private val _instance = new StaticRulesImpl()
  def instance() =
    _instance

  class StaticRulesImpl() extends StaticRules {
    var entityKinds: Set[String] = Set.empty
    override def getValues(): Set[String] = entityKinds
    override def addValues(kinds: Set[String]): Unit = entityKinds ++= kinds
    override def prova(): Unit = entityKinds foreach println
  }
}


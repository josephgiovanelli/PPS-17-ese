package it.unibo.pps.ese.entitybehaviors.decisionsupport

import it.unibo.pps.ese.entitybehaviors.StaticRules
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityAttributesImpl.EntityAttributesImpl
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl

/*case class MyInt(int: Int) {
  def meno(i: Int): MyInt = MyInt(int - i)
  def piu(i: Int): MyInt = MyInt(int + i)
  def >(i: MyInt): Boolean = int > i.int
  def <(i: MyInt): Boolean = int < i.int
  def toInt: Int = int
}*/

trait WorldTypes {
  type Name
  type Kind
  type HeightMeasure <: Int
  type AttackMeasure <: Int
  type PositionMeasure <: Int

  type EntityAttributes <: {
    def name: Name
    def kind: Kind
    def height: HeightMeasure
    def strong: AttackMeasure
    def defense: AttackMeasure
    def position: GeneralPosition[PositionMeasure]
  }

  type EntityChoice <: {
    def name: Name
    def distance: Int
  }

  type WorldRules <: {
    def attackThreshold: AttackMeasure
    def heightThresholds: (HeightMeasure, HeightMeasure)
    def compatibleHuntingKinds: Set[(Kind, Kind)]
    def compatibleCouplingKinds: Set[(Kind, Kind)]
  }
}

trait WorldTypesImpl extends WorldTypes {
  type Name = Int
  type Kind = EntityKinds.Value
  type HeightMeasure = Int
  type AttackMeasure = Int
  type PositionMeasure = Int

  type EntityAttributes = EntityAttributesImpl
  type EntityChoice = EntityChoiceImpl
  type WorldRules = WorldRulesImpl

  implicit def generalPositionToTuple(generalPosition: GeneralPosition[Int]): (Int, Int) = (generalPosition.x, generalPosition.y)
  implicit def tupleToGeneralPosition(tuple: (Int, Int)): GeneralPosition[Int] = GeneralPosition(tuple._1, tuple._2)
}

object EntityKinds extends Enumeration {
  type EntityKinds = Value
  val entityKinds: Set[String] = StaticRules.instance().getSpecies()
  entityKinds.foreach(Value)

  private val constants: Map[Symbol, EntityKinds.Value] = entityKinds.map(v => Symbol(v) -> withName(v)).toMap
  def apply(c: Symbol): EntityKinds = constants(c)
  def unapply(arg: EntityKinds): Option[Symbol] = Some(Symbol(values.find(x => arg.equals(x)).get.toString))
}

case class GeneralPosition[PositionMeasure <: Int](x: PositionMeasure, y: PositionMeasure) {
  def sameAbscissa(generalPosition: GeneralPosition[PositionMeasure]): Int = if (x == generalPosition.x) 0 else if (x > generalPosition.x) 1 else -1
  def sameOrdinate(generalPosition: GeneralPosition[PositionMeasure]): Int = if (y == generalPosition.y) 0 else if (y > generalPosition.y) 1 else -1
}


object EntityAttributesImpl {
  def apply(name: Int, kind: EntityKinds.Value, height: Int, strong: Int, defense: Int, position: GeneralPosition[Int]): EntityAttributesImpl = EntityAttributesImpl(name, kind, height, strong, defense, position)

  implicit def generalPositionToTuple(generalPosition: GeneralPosition[Int]): (Int, Int) = (generalPosition.x, generalPosition.y)
  implicit def tupleToGeneralPosition(tuple: (Int, Int)): GeneralPosition[Int] = GeneralPosition(tuple._1, tuple._2)

  case class EntityAttributesImpl(name: Int, kind: EntityKinds.Value, height: Int, strong: Int, defense: Int, position: GeneralPosition[Int]){
    override def toString: String = "Entity(" + name + ", " + kind + ", " + height + ", " + strong + ", " + defense + ", [" + position.x + ", " + position.y + "])"
  }

}

case class EntityChoiceImpl(name: Int, distance: Int)

object WorldRulesImpl {
  def apply(attackThreshold: Int, heightThresholds: (Int, Int), compatibleHuntingKinds: Set[(EntityKinds.Value, EntityKinds.Value)], compatibleCouplingKinds: Set[(EntityKinds.Value, EntityKinds.Value)]): WorldRulesImpl =  WorldRulesImpl(attackThreshold, heightThresholds, compatibleHuntingKinds, compatibleCouplingKinds)
  implicit def stringToEntityKinds(string: String): EntityKinds.Value = EntityKinds(Symbol(string))
  implicit def tupleStringToEntityKinds(tuple: (String, String)): (EntityKinds.Value, EntityKinds.Value) = (tuple._1, tuple._2)
  implicit def setTupleStringToSetTupleEntityKinds(set: Set[(String, String)]): Set[(EntityKinds.Value, EntityKinds.Value)] = set map tupleStringToEntityKinds

  case class WorldRulesImpl(attackThreshold: Int, heightThresholds: (Int, Int), compatibleHuntingKinds: Set[(EntityKinds.Value, EntityKinds.Value)], compatibleCouplingKinds: Set[(EntityKinds.Value, EntityKinds.Value)])
}


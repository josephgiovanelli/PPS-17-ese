package it.unibo.pps.ese.entitybehaviors.decisionsupport

import it.unibo.pps.ese.entitybehaviors.StaticRules
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityAttributesImpl.EntityAttributesImpl
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityKinds.entityKinds
import it.unibo.pps.ese.entitybehaviors.decisionsupport.WorldRulesImpl.WorldRulesImpl


trait WorldTypes {
  type Name
  type Kind
  type HeightMeasure
  type AttackMeasure
  type PositionMeasure <: Int
  type AttractivenessMeasure
  type Sex

  type EntityAttributes <: {
    def name: Name
    def kind: Kind
    def height: HeightMeasure
    def strong: AttackMeasure
    def defense: AttackMeasure
    def position: GeneralPosition[PositionMeasure]
    def attractiveness: AttractivenessMeasure
    def sex: Sex
  }

  type EntityChoice <: {
    def name: Name
    def distance: PositionMeasure
  }

  type WorldRules <: {
    def attackThreshold: AttackMeasure
    def heightThresholds: HeightMeasure
    def compatibleHuntingKinds: Set[(Kind, Kind)]
    def compatibleCouplingKinds: Set[(Kind, Kind)]
  }
}

trait WorldTypesImpl extends WorldTypes {
  type Name = String
  type Kind = EntityKinds.Value
  type HeightMeasure = Double
  type AttackMeasure = Double
  type PositionMeasure = Int
  type AttractivenessMeasure = Double
  type Sex = SexTypes.Value

  type EntityAttributes = EntityAttributesImpl
  type EntityChoice = EntityChoiceImpl
  type WorldRules = WorldRulesImpl

  implicit def generalPositionToTuple(generalPosition: GeneralPosition[Int]): (Int, Int) = (generalPosition.x, generalPosition.y)
  implicit def tupleToGeneralPosition(tuple: (Int, Int)): GeneralPosition[Int] = GeneralPosition(tuple._1, tuple._2)
}

object SexTypes extends Enumeration {
  val male, female = Value

  def withNameOpt(s: String): Option[Value] = values.find(_.toString == s)
}

object EntityKinds extends Enumeration {
  type EntityKinds = Value
  var entityKinds: Set[String] = StaticRules.instance().getSpecies()
  entityKinds.foreach(Value)
  private var constants: Map[Symbol, EntityKinds.Value] = entityKinds.map(v => Symbol(v) -> withName(v)).toMap

  def updateSpecies(): Unit = {
    entityKinds = StaticRules.instance().getSpecies()
    entityKinds.foreach(Value)
    constants = entityKinds.map(v => Symbol(v) -> withName(v)).toMap
  }
  def apply(c: Symbol): EntityKinds = constants(c)
  def unapply(arg: EntityKinds): Option[Symbol] = Some(Symbol(values.find(x => arg.equals(x)).get.toString))
}

case class GeneralPosition[PositionMeasure <: Int](x: PositionMeasure, y: PositionMeasure) {
  def sameAbscissa(generalPosition: GeneralPosition[PositionMeasure]): Int = if (x == generalPosition.x) 0 else if (x > generalPosition.x) 1 else -1
  def sameOrdinate(generalPosition: GeneralPosition[PositionMeasure]): Int = if (y == generalPosition.y) 0 else if (y > generalPosition.y) 1 else -1
}

object AnimalAttributes {
  def apply(name: String, kind: EntityKinds.Value, height: Double, strong: Double, defense: Double, position: GeneralPosition[Int], attractiveness: Double, sex: SexTypes.Value): EntityAttributesImpl = EntityAttributesImpl(name, kind, height, strong, defense, position, attractiveness, sex)
}

object PlantAttributes {
  def apply(name: String, kind: EntityKinds.Value, height: Double, defense: Double, position: GeneralPosition[Int], sex: SexTypes.Value): EntityAttributesImpl = EntityAttributesImpl(name, kind, height, 0, defense, position, 0, sex)
}

object EntityAttributesImpl {
  def apply(name: String, kind: EntityKinds.Value, height: Double, strong: Double, defense: Double, position: GeneralPosition[Int], attractiveness: Double, sex: SexTypes.Value): EntityAttributesImpl = EntityAttributesImpl(name, kind, height, strong, defense, position, attractiveness, sex)

  implicit def generalPositionToTuple(generalPosition: GeneralPosition[Int]): (Int, Int) = (generalPosition.x, generalPosition.y)
  implicit def tupleToGeneralPosition(tuple: (Int, Int)): GeneralPosition[Int] = GeneralPosition(tuple._1, tuple._2)

  case class EntityAttributesImpl(name: String, kind: EntityKinds.Value, height: Double, strong: Double, defense: Double, var position: GeneralPosition[Int], attractiveness: Double, sex: SexTypes.Value){
    override def toString: String = "Entity(" + name + ", " + kind + ", " + height + ", " + strong + ", " + defense + ", [" + position.x + ", " + position.y + "], " + attractiveness + ", " + sex + ")"
  }

}

case class EntityChoiceImpl(name: String, distance: Int)

object WorldRulesImpl {
  /**
    *
    * @param attackThreshold
    * @param heightThresholds
    * @param couplingThreshold
    * @param compatibleHuntingKinds
    * @param compatibleCouplingKinds
    * @return
    */
  def apply(attackThreshold: Double, heightThresholds: Double, couplingThreshold: Double, compatibleHuntingKinds: Set[(EntityKinds.Value, EntityKinds.Value)], compatibleCouplingKinds: Set[(EntityKinds.Value, EntityKinds.Value)]): WorldRulesImpl =  WorldRulesImpl(attackThreshold, heightThresholds, couplingThreshold, compatibleHuntingKinds, compatibleCouplingKinds)
  implicit def stringToEntityKinds(string: String): EntityKinds.Value = EntityKinds(Symbol(string))
  implicit def tupleStringToEntityKinds(tuple: (String, String)): (EntityKinds.Value, EntityKinds.Value) = (tuple._1, tuple._2)
  implicit def setTupleStringToSetTupleEntityKinds(set: Set[(String, String)]): Set[(EntityKinds.Value, EntityKinds.Value)] = set map tupleStringToEntityKinds

  /**
    *
    * @param attackThreshold
    * @param heightThresholds
    * @param couplingThreshold
    * @param compatibleHuntingKinds
    * @param compatibleCouplingKinds
    */
  case class WorldRulesImpl(attackThreshold: Double, heightThresholds: Double, couplingThreshold: Double, var compatibleHuntingKinds: Set[(EntityKinds.Value, EntityKinds.Value)] = Set.empty, var compatibleCouplingKinds: Set[(EntityKinds.Value, EntityKinds.Value)] = Set.empty) {
    def setCompatibleHuntingKinds(set: Set[(String, String)]): Unit = compatibleHuntingKinds = set
    def setCompatibleCouplingKinds(set: Set[(String, String)]): Unit = compatibleCouplingKinds = set
  }
}


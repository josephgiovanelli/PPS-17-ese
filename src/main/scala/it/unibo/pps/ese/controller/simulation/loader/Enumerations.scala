package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.model.genetics.entities.QualityType

/** Trait that defines base enum element*/
trait EnumElem

/** Trait that defines a simple enum*/
trait MyEnum[T <: EnumElem] {
  /**
    * @return All enum's elements
    */
  def elements: Set[T]
}

/** Trait that defines a enum element that contains base information regarding a default gene*/
abstract class DefaultGene extends EnumElem {
  /** Gene name*/
  def name: String
  /** Gene properties*/
  def properties: Map[String, Class[_]]


  def canEqual(other: Any): Boolean = other.isInstanceOf[DefaultGene]

  override def equals(other: Any): Boolean = other match {
    case that: DefaultGene =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/** Abstract class that represent a enum element that contains base information regarding a regulation default gene*/
sealed abstract class RegulationDefaultGene (override val name: String,
                                             override val properties : Map[String, Class[_]]) extends DefaultGene

/** Custom enum that contains all regulation genes that an animal must have*/
object RegulationDefaultGenes extends MyEnum[RegulationDefaultGene] {

  case object LIFE extends RegulationDefaultGene("life", Map(("life", classOf[Double])))
  case object CHILDHOOD extends RegulationDefaultGene("childhood", Map(("childhood", classOf[Double])))
  case object MATURITY extends RegulationDefaultGene("maturity", Map(("maturity", classOf[Double])))
  case object OLDNESS extends RegulationDefaultGene("oldness", Map(("oldness", classOf[Double])))
  case object DECLINE extends RegulationDefaultGene("decline", Map(("decline", classOf[Double])))

  val elements: Set[RegulationDefaultGene] = Set(LIFE, CHILDHOOD, MATURITY, OLDNESS, DECLINE)
}

/** Abstract class that represent a enum element that contains base information regarding a sexual default gene*/
sealed abstract class SexualDefaultGene (override val name: String,
                                             override val properties : Map[String, Class[_]]) extends DefaultGene

/** Custom enum that contains all sexual genes that an animal must have*/
object SexualDefaultGenes extends MyEnum[SexualDefaultGene] {

  case object FERTILITY extends SexualDefaultGene("fertility", Map(("fertility", classOf[Double])))
  case object FECUNDITY extends SexualDefaultGene("fecundity", Map(("fecundity", classOf[Double])))
  case object PREGNANCY_DURATION extends SexualDefaultGene("pregnancyDuration", Map(("pregnancyDuration", classOf[Double])))

  val elements: Set[SexualDefaultGene] = Set(FERTILITY, FECUNDITY, PREGNANCY_DURATION)
}

/** Trait that defines a enum element that contains a base code*/
sealed trait Code[X] extends EnumElem {
  val code: X
}

/** Abstract class that represent an animal typology*/
sealed abstract class Typology(override val code: String) extends Code[String]

/** Custom enum that contains all possible typologies*/
object Typologies extends MyEnum[Typology] {
  case object CARNIVOROUS extends Typology("C")
  case object HERBIVOROUS extends Typology("H")

  val elements: Set[Typology] = Set(CARNIVOROUS, HERBIVOROUS)
}

/** Abstract class that represent an entity reign*/
sealed abstract class Reign(override val code: String) extends Code[String]

/** Custom enum that contains all possible reigns*/
object Reigns extends MyEnum[Reign] {
  case object ANIMALS extends Reign("A")
  case object PLANTS extends Reign("P")

  val elements: Set[Reign] = Set(ANIMALS, PLANTS)
}

/** Class that defines an enum element representing an entity predefined property*/
sealed case class Property(name: String, requiredType: Class[_]) extends EnumElem

/** Custom enum that contains all animals properties*/
object AnimalStructuralProperties extends MyEnum[Property] {
  override def elements: Set[Property] = QualityType.animalStructuralQualities.map(q =>
    Property(EnumUtils.nameNormalize(q.entryName), classOf[Double])).toSet
}

private object EnumUtils {
  def nameNormalize(name: String): String = name.charAt(0).toLower + name.drop(1)
}
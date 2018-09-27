package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.model.genetics.entities.QualityType

trait EnumElem

trait MyEnum[T <: EnumElem] {
  def elements: Set[T]
}

abstract class DefaultGene extends EnumElem {
  def name: String
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

sealed abstract class RegulationDefaultGene (override val name: String,
                                             override val properties : Map[String, Class[_]]) extends DefaultGene

object RegulationDefaultGenes extends MyEnum[RegulationDefaultGene] {

  case object LIFE extends RegulationDefaultGene("life", Map(("life", classOf[Double])))
  case object CHILDHOOD extends RegulationDefaultGene("childhood", Map(("childhood", classOf[Double])))
  case object MATURITY extends RegulationDefaultGene("maturity", Map(("maturity", classOf[Double])))
  case object OLDNESS extends RegulationDefaultGene("oldness", Map(("oldness", classOf[Double])))
  case object DECLINE extends RegulationDefaultGene("decline", Map(("decline", classOf[Double])))

  val elements: Set[RegulationDefaultGene] = Set(LIFE, CHILDHOOD, MATURITY, OLDNESS, DECLINE)
}

sealed abstract class SexualDefaultGene (override val name: String,
                                             override val properties : Map[String, Class[_]]) extends DefaultGene

object SexualDefaultGenes extends MyEnum[SexualDefaultGene] {

  case object FERTILITY extends SexualDefaultGene("fertility", Map(("fertility", classOf[Double])))
  case object FECUNDITY extends SexualDefaultGene("fecundity", Map(("fecundity", classOf[Double])))
  case object PREGNANCY_DURATION extends SexualDefaultGene("pregnancyDuration", Map(("pregnancyDuration", classOf[Double])))

  val elements: Set[SexualDefaultGene] = Set(FERTILITY, FECUNDITY, PREGNANCY_DURATION)
}

sealed abstract class Code(val code: String) extends EnumElem
object Typologies extends MyEnum[Code] {
  case object CARNIVOROUS extends Code("C")
  case object HERBIVOROUS extends Code("H")

  val elements: Set[Code] = Set(CARNIVOROUS, HERBIVOROUS)
}
object Reigns extends MyEnum[Code] {
  case object ANIMALS extends Code("A")
  case object PLANTS extends Code("P")

  val elements: Set[Code] = Set(ANIMALS, PLANTS)
}

sealed case class Property(name: String, requiredType: Class[_]) extends EnumElem

object AnimalStructuralProperties extends MyEnum[Property] {
  override def elements: Set[Property] = QualityType.animalStructuralQualities.map(q =>
    Property(EnumUtils.nameNormalize(q.entryName), classOf[Double])).toSet
}

private object EnumUtils {
  def nameNormalize(name: String): String = name.charAt(0).toLower + name.drop(1)
}
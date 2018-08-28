package it.unibo.pps.ese.controller.loader

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

  val elements: Set[SexualDefaultGene] = Set(FERTILITY, FECUNDITY)
}
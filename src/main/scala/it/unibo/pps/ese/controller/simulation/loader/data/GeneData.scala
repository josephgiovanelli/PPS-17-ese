package it.unibo.pps.ese.controller.simulation.loader.data

/** Trait that defines gene's data. All non-mandatory fields can be not filled, so are represented by an option
  *
  * @tparam A Internal alleles' type(completion level)
  */
trait GeneData[+A <: PartialAlleleData] {
  /**Gene's name*/
  val name: String
  /**Gene's id*/
  val getId: Option[String]
  /**Gene's properties, represented with a map containing property's name as key and type as value*/
  val getProperties: Option[Map[String, Class[_]]]
  /**Gene's alleles*/
  val getAlleles: Option[Iterable[A]]

  def canEqual(other: Any): Boolean = other.isInstanceOf[GeneData[_]]

  override def equals(other: Any): Boolean = other match {
    case that: GeneData[_] =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/**Rich trait that defines full gene's data, where all non-mandatory fields must be filled. Trait can be directly
  * mixed with a [[it.unibo.pps.ese.controller.simulation.loader.data.GeneData]]
  */
trait FullGeneData[A <: PartialAlleleData] extends GeneData[A] {
  /**
    * @throws IllegalStateException if property is not set
    * @return Gene's id
    */
  @throws[IllegalStateException]
  def id: String = getId.getOrElse(throw new IllegalStateException())
  /**
    * @throws IllegalStateException if property is not set
    * @return Gene's properties, represented with a map containing property's name as key and type as value
    */
  @throws[IllegalStateException]
  def properties: Map[String, Class[_]] = getProperties.getOrElse(throw new IllegalStateException())
  /**
    * @throws IllegalStateException if property is not set
    * @return Gene's alleles
    */
  @throws[IllegalStateException]
  def alleles: Set[A] = getAlleles.getOrElse(throw new IllegalStateException()).toSet
}

object GeneData {
  /** Type that defines partial gene data's features*/
  type PartialGeneData = GeneData[_ <: PartialAlleleData]
  /** Type that defines complete animal data's features*/
  type CompleteGeneData = FullGeneData[CompleteAlleleData]
}


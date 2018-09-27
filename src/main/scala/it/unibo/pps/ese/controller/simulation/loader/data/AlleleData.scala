package it.unibo.pps.ese.controller.simulation.loader.data

/** Trait that defines allele's data. All non-mandatory fields can be not filled, so are represented by an option*/
trait PartialAlleleData {
  /**Allele's id*/
  val id: String
  /** Allele's gene id*/
  val getGene: Option[String]
  /** Allele's dominance level*/
  val getDominance: Option[Double]
  /** Allele's energetic consume*/
  val getConsume: Option[Double]
  /** Allele's spread probability*/
  val getProbability: Option[Double]
  /** Allele's effect to gene's properties, represented with a map containing property's name as key and numeric effect
    * as value
    */
  val getEffect: Option[Map[String, Double]]

  def canEqual(other: Any): Boolean = other.isInstanceOf[PartialAlleleData]

  override def equals(other: Any): Boolean = other match {
    case that: PartialAlleleData =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/** Rich trait that defines complete allele's data, where all non-mandatory fields must be filled. Trait can be directly mixed with a
  * [[it.unibo.pps.ese.controller.simulation.loader.data.PartialAlleleData]]
  *
  */
trait CompleteAlleleData extends PartialAlleleData {
  /**
    * @throws IllegalStateException if property is not set
    * @return Allele's gene id
    */
  @throws[IllegalStateException]
  def gene: String = getGene.getOrElse(throw new IllegalStateException)
  /**
    * @throws IllegalStateException if property is not set
    * @return Allele's dominance level
    */
  @throws[IllegalStateException]
  def dominance: Double = getDominance.getOrElse(throw new IllegalStateException)
  /**
    * @throws IllegalStateException if property is not set
    * @return Allele's energetic consume
    */
  @throws[IllegalStateException]
  def consume: Double = getConsume.getOrElse(throw new IllegalStateException)
  /**
    * @throws IllegalStateException if property is not set
    * @return Allele's spread probability
    */
  @throws[IllegalStateException]
  def probability: Double = getProbability.getOrElse(throw new IllegalStateException)
  /**
    * @throws IllegalStateException if property is not set
    * @return Allele's effect to gene's properties, represented with a map containing property's name as key and numeric effect
    *         as value
    */
  @throws[IllegalStateException]
  def effect: Map[String, Double] = getEffect.getOrElse(throw new IllegalStateException)
}

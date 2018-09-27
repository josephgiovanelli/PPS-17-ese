package it.unibo.pps.ese.controller.simulation.loader.data

/** Trait that defines entity's data. All non-mandatory fields can be not filled, so are represented by an option*/
trait EntityData {
  /**Entity's name*/
  val name: String
  /**Entity's genes' id length*/
  val getGeneLength: Option[Int]
  /**Entity's alleles' id length*/
  val getAlleleLength: Option[Int]
  /**Entity's reign*/
  val getReign: Option[String]

  def canEqual(other: Any): Boolean = other.isInstanceOf[EntityData]

  override def equals(other: Any): Boolean = other match {
    case that: EntityData =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/**Rich trait that defines full entity's data, where all non-mandatory fields must be filled. Trait can be directly
  * mixed with a[[it.unibo.pps.ese.controller.simulation.loader.data.EntityData]]
  */
trait FullEntityData extends EntityData {
  /**
    * @throws IllegalStateException if property is not set
    * @return Entity's genes' id length
    */
  @throws[IllegalStateException]
  def geneLength: Int = getGeneLength.getOrElse(throw new IllegalStateException())
  /**
    * @throws IllegalStateException if property is not set
    * @return Entity's alleles' id length
    */
  @throws[IllegalStateException]
  def alleleLength: Int = getAlleleLength.getOrElse(throw new IllegalStateException())
  /**
    * @throws IllegalStateException if property is not set
    * @return Entity's reign
    */
  @throws[IllegalStateException]
  def reign: String = getReign.getOrElse(throw new IllegalStateException())
}

object EntityData {
  /** Type that defines partial entity data's features*/
  type PartialEntityData = EntityData
  /** Type that defines complete entity data's features*/
  type CompleteEntityData = FullEntityData
}
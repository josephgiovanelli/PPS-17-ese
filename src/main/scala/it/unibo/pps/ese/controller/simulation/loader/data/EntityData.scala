package it.unibo.pps.ese.controller.simulation.loader.data

trait EntityData {
  val name: String
  def getGeneLength: Option[Int]
  def getAlleleLength: Option[Int]
  def getReign: Option[String]

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

trait FullEntityData extends EntityData {
  def geneLength: Int = getGeneLength.getOrElse(throw new IllegalStateException())
  def alleleLength: Int = getAlleleLength.getOrElse(throw new IllegalStateException())
  def reign: String = getReign.getOrElse(throw new IllegalStateException())
}

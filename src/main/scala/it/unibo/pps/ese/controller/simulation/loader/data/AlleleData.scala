package it.unibo.pps.ese.controller.simulation.loader.data

trait PartialAlleleData {
  val id: String
  def getGene: Option[String]
  def getDominance: Option[Double]
  def getConsume: Option[Double]
  def getProbability: Option[Double]
  def getEffect: Option[Map[String, Double]]

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

trait CompleteAlleleData extends PartialAlleleData {
  def gene: String = getGene.getOrElse(throw new IllegalStateException)
  def dominance: Double = getDominance.getOrElse(throw new IllegalStateException)
  def consume: Double = getConsume.getOrElse(throw new IllegalStateException)
  def probability: Double = getProbability.getOrElse(throw new IllegalStateException)
  def effect: Map[String, Double] = getEffect.getOrElse(throw new IllegalStateException)
}

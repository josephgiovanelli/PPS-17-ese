package it.unibo.pps.ese.controller.loader.beans

import it.unibo.pps.ese.controller.loader.data.AlleleData

case class Allele(gene: String,
                  id: String,
                  dominance: Double,
                  consume: Double,
                  probability: Double,
                  effect: Map[String, Double]) extends AlleleData {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Allele]

  override def equals(other: Any): Boolean = other match {
    case that: Allele =>
      (that canEqual this) &&
        gene == that.gene &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(gene, id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

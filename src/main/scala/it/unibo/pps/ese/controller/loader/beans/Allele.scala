package it.unibo.pps.ese.controller.loader.beans

import it.unibo.pps.ese.controller.loader.data.CompleteAlleleData

case class Allele(gene: Option[String],
                  id: Option[String],
                  dominance: Option[Double],
                  consume: Option[Double],
                  probability: Option[Double],
                  effect: Option[Map[String, Double]]) {
//  def canEqual(other: Any): Boolean = other.isInstanceOf[Allele]
//
//  override def equals(other: Any): Boolean = other match {
//    case that: Allele =>
//      (that canEqual this) &&
//        gene == that.gene &&
//        id == that.id
//    case _ => false
//  }
//
//  override def hashCode(): Int = {
//    val state = Seq(gene, id)
//    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
//  }
}

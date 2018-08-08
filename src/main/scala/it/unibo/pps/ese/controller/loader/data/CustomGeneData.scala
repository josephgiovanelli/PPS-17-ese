package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Gene

trait CustomGeneData extends GeneData {
  def conversionMap: Map[String, Map[String, Double]]
}

object CustomGeneData {
  def apply(gene: Gene, alleles: Seq[AlleleData]): CustomGeneData = ???
}
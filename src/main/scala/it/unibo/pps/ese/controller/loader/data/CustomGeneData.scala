package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Gene

trait CustomGeneData extends GeneData {
  def conversionMap: Map[String, Map[String, Double]]
}

object CustomGeneData {
  def apply(gene: Gene, alleles: Seq[AlleleData]): CustomGeneData = new CustomGeneDataImpl(gene, alleles)

  private class CustomGeneDataImpl(gene: Gene, alleles: Seq[AlleleData])
    extends {
      val id: String = gene.id
  } with AbsGeneData(gene.properties.keySet.map((_, classOf[Double])).toMap, alleles) with CustomGeneData {
    val name: String = gene.simpleName
    val conversionMap: Map[String, Map[String, Double]] = gene.properties.map({case (k,v) => (k, v.conversionMap)})
    //TODO check conversion map with qualità base
  }
}
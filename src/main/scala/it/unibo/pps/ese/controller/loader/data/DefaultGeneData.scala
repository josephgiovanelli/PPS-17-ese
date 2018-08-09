package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.DefaultGene

trait DefaultGeneData extends GeneData

object DefaultGeneData {
  def apply(defaultGene: DefaultGene, id: String, alleleData: Seq[AlleleData] = Seq()): DefaultGeneData
  = new DefaultGeneDataImpl(defaultGene, id, alleleData)

  private class DefaultGeneDataImpl(defaultGene: DefaultGene, _id: String, _alleles: Seq[AlleleData]) extends {
    override val id: String = _id
    override val name: String = defaultGene.name
  } with AbsGeneData(defaultGene.properties, _alleles) with DefaultGeneData
}

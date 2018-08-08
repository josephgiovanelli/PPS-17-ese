package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.DefaultGene

trait DefaultGeneData extends GeneData

object DefaultGeneData {
  def apply(defaultGene: DefaultGene, id: String, alleleData: Seq[AlleleData] = Seq()): GeneData
  = new DefaultGeneDataImpl(defaultGene, id, alleleData)

  private class DefaultGeneDataImpl(defaultGene: DefaultGene, override val id: String,
                                    override val alleles: Seq[AlleleData])
                                  extends AbsGeneData(defaultGene.properties, alleles) with DefaultGeneData {
    override val name: String = defaultGene.name
  }
}

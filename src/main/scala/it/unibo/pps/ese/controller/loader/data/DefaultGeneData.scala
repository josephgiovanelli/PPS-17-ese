package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.DefaultGene

trait DefaultGeneData extends GeneData
trait PartialDefaultGeneData extends DefaultGeneData
trait CompleteDefaultGeneData extends PartialDefaultGeneData

object DefaultGeneData {
  def apply(defaultGene: DefaultGene, id: String, alleleData: Iterable[AlleleData] = Seq()): CompleteDefaultGeneData
  = new DefaultGeneDataImpl(id, defaultGene.name, defaultGene.properties, alleleData) with CompleteDefaultGeneData

  def apply(name: String, id: String, properties: Map[String, Class[_]], alleleData: Iterable[AlleleData]): DefaultGeneData
  = new DefaultGeneDataImpl(id, name, properties, alleleData)

  class DefaultGeneDataImpl(_id: String, _name: String, _properties: Map[String, Class[_]],
                                    _alleles: Iterable[AlleleData]) extends {
    override val id: String = _id
    override val name: String = _name
  } with AbsGeneData(_properties, _alleles) with DefaultGeneData
}

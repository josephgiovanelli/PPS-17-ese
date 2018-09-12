package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.DefaultGene

trait PartialDefaultGeneData extends PartialGeneData
trait CompleteDefaultGeneData extends PartialDefaultGeneData with CompleteGeneData

object DefaultGeneData {
  def apply(defaultGene: DefaultGene, id: String, alleleData: Iterable[AlleleData] = Seq()): CompleteDefaultGeneData
  = new DefaultGeneDataImpl(id, defaultGene.name, defaultGene.properties, alleleData) with CompleteDefaultGeneData

  class DefaultGeneDataImpl(_id: String, _name: String, _properties: Map[String, Class[_]],
                                    _alleles: Iterable[AlleleData]) extends PartialGeneData {
    override def getId: Option[String] = Some(_id)

    override def getName: Option[String] = Some(_name)

    override def getProperties: Option[Map[String, Class[_]]] = Some(_properties)

    //TODO to set?????
    override def getAlleles: Option[Set[AlleleData]] = Some(_alleles.toSet)
  }
}

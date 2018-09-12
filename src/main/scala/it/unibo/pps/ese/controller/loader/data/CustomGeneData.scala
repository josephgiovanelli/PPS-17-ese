package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Gene
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.DefaultGeneDataImpl

trait PartialCustomGeneData extends PartialGeneData {
  def getConversionMap: Option[Map[String, Map[String, Double]]]
}
trait CompleteCustomGeneData extends PartialCustomGeneData with CompleteGeneData {
  def conversionMap: Map[String, Map[String, Double]] = getConversionMap.getOrElse(throw new IllegalStateException())
}

object CustomGeneData {
  def apply(gene: Gene, alleles: Iterable[AlleleData]): CompleteCustomGeneData =
    new CustomGeneDataImpl(gene.id, gene.simpleName, gene.properties.keySet.map((_, classOf[Double])).toMap, alleles,
      gene.properties.map({case (k,v) => (k, v.conversionMap)})) with CompleteCustomGeneData

  class CustomGeneDataImpl(_id: String, _name: String, _properties: Map[String, Class[_]],
                                   _alleleData: Iterable[AlleleData], _conversionMap: Map[String, Map[String, Double]])
    extends DefaultGeneDataImpl(_id, _name, _properties, _alleleData) with PartialCustomGeneData {
    val getConversionMap: Option[Map[String, Map[String, Double]]] = Some(_conversionMap)
    //TODO check conversion map with base qualities
  }
}
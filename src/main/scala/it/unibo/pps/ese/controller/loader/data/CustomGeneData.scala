package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Gene

trait CustomGeneData extends GeneData {
  def conversionMap: Map[String, Map[String, Double]]
}

trait PartialCustomGeneData extends CustomGeneData
trait CompleteCustomGeneData extends PartialCustomGeneData

object CustomGeneData {
  def apply(gene: Gene, alleles: Iterable[AlleleData]): CompleteCustomGeneData =
    new CustomGeneDataImpl(gene.id, gene.simpleName, gene.properties.keySet.map((_, classOf[Double])).toMap, alleles,
      gene.properties.map({case (k,v) => (k, v.conversionMap)})) with CompleteCustomGeneData

  def apply(name: String, id: String, properties: Map[String, Class[_]], alleleData: Iterable[AlleleData],
            conversionMap: Map[String, Map[String, Double]]): CustomGeneData
  = new CustomGeneDataImpl(id, name, properties, alleleData, conversionMap)

  class CustomGeneDataImpl(_id: String, _name: String, _properties: Map[String, Class[_]],
                                   _alleleData: Iterable[AlleleData], _conversionMap: Map[String, Map[String, Double]])
    extends {
      val id: String = _id
  } with AbsGeneData(_properties, _alleleData) with CustomGeneData {
    val name: String = _name
    val conversionMap: Map[String, Map[String, Double]] = _conversionMap
    //TODO check conversion map with base qualities
  }
}
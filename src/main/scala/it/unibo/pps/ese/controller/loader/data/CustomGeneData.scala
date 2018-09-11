package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.beans.Gene

trait CustomGeneData extends GeneData {
  def conversionMap: Map[String, Map[String, Double]]
}

trait CompleteCustomGeneData extends CustomGeneData
trait PartialCustomGeneData extends CustomGeneData

object CustomGeneData {
  def apply(gene: Gene, alleles: Iterable[AlleleData]): CustomGeneData =
    new CustomGeneDataImpl(gene.simpleName, gene.id, gene.properties.keySet.map((_, classOf[Double])).toMap, alleles,
      gene.properties.map({case (k,v) => (k, v.conversionMap)}))

  def apply(name: String, id: String, properties: Map[String, Class[_]], alleleData: Iterable[AlleleData],
            conversionMap: Map[String, Map[String, Double]]): CustomGeneData
  = new CustomGeneDataImpl(name, id, properties, alleleData, conversionMap)

  class CustomGeneDataImpl(_name: String, _id: String, _properties: Map[String, Class[_]],
                                   _alleleData: Iterable[AlleleData], _conversionMap: Map[String, Map[String, Double]])
    extends {
      val id: String = _id
  } with AbsGeneData(_properties, _alleleData) with CustomGeneData {
    val name: String = _name
    val conversionMap: Map[String, Map[String, Double]] = _conversionMap
    //TODO check conversion map with base qualities
  }
}
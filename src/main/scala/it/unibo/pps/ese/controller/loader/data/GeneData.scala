package it.unibo.pps.ese.controller.loader.data

trait GeneData {
  def id: String
  def name: String
  def properties: Map[String, Class[_]]
  def alleles: Seq[AlleleData]
}

abstract class AbsGeneData(val properties: Map[String, Class[_]], val alleles: Seq[AlleleData]) extends GeneData {
  def id: String
  def name: String
  require(alleles.forall(data => data.effect.keySet.subsetOf(properties.keySet)))
}

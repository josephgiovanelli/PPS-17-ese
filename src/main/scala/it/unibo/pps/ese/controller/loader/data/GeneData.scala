package it.unibo.pps.ese.controller.loader.data

trait GeneData {
  def id: String
  def name: String
  def properties: Map[String, Class[_]]
  def alleles: Seq[AlleleData]
}

abstract class AbsGeneData(val properties: Map[String, Class[_]], val alleles: Seq[AlleleData]) extends GeneData {
  def name: String
  alleles.foreach(
    all => {
      require(all.effect.keySet.subsetOf(properties.keySet),
        "Allele " + all.id + " of gene " + id + " (simple name: " + name + " )" +
          "tries to modify properties: " + all.effect.keySet.toString()
          + ", but the gene has only properties: "
          + properties.keySet.toString())
      require(all.gene == id, all.gene + " " + id)
    }
  )
}

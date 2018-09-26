package it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals

/**
  * It describes an allele.
  * @param gene the gene name
  * @param id the allele identifier
  * @param dominance the dominance of the allele
  * @param consume the consume of the allele
  * @param probability the probability of manifestation of the allele
  * @param effect the effects of the allele
  */
case class AlleleInfo(gene: String,
                      id: String,
                      dominance: Double,
                      consume: Double,
                      probability: Double,
                      var effect: Map[String, Double])
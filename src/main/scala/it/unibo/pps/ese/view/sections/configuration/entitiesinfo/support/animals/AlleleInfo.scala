package it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals

case class AlleleInfo(gene: String,
                      id: String,
                      dominance: Double,
                      consume: Double,
                      probability: Double,
                      var effect: Map[String, Double])
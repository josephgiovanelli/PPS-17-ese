package it.unibo.pps.ese.controller.simulation.loader.beans

/** Simple bean used for YAML deserialization*/
case class Allele(gene: Option[String],
                  id: String,
                  dominance: Option[Double],
                  consume: Option[Double],
                  probability: Option[Double],
                  effect: Option[Map[String, Double]])

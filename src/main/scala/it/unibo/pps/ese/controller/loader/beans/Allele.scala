package it.unibo.pps.ese.controller.loader.beans

import it.unibo.pps.ese.controller.loader.data.AlleleData

case class Allele(gene: String, id: String, dominance: Double, consume: Double,
                  probability: Double, effect: Map[String, Double]) extends AlleleData

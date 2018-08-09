package it.unibo.pps.ese.controller.loader.beans

import it.unibo.pps.ese.controller.loader.data.PlantData

case class Plant(name: String, geneLength: Int, reign: String, height: Double,
                 nutritionalValue: Double, availability: Double) extends PlantData

package it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals

import it.unibo.pps.ese.controller.loader.DefaultGene

class GeneInfo(val id: String,
               val name: String,
               val properties: Map[String, Class[_]])

case class DefaultGeneInfo(defaultGene: DefaultGene,
                           override val id: String) extends GeneInfo(id, defaultGene.name, defaultGene.properties)

case class CustomGeneInfo(override val id: String,
                          override val name: String,
                          override val  properties: Map[String, Class[_]],
                          conversionMap: Map[String, Map[String, Double]]) extends GeneInfo(id, name, properties)

case class ConversionMap(property: String,
                         map: Map[String, Double])

package it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals

import it.unibo.pps.ese.controller.simulation.loader.DefaultGene

/**
  * It describes a general gene.
  * @param id the gene identifier
  * @param name the gene name
  * @param properties the gene properties
  */
class GeneInfo(val id: String,
               val name: String,
               val properties: Map[String, Class[_]])

/**
  * It describes a [[DefaultGeneInfo]].
  * @param defaultGene the default gene to catch the base information
  * @param id the gene identifier
  */
case class DefaultGeneInfo(defaultGene: DefaultGene,
                           override val id: String) extends GeneInfo(id, defaultGene.name, defaultGene.properties)

/**
  * It describes a [[CustomGeneInfo()]].
  * @param id the gene identifier
  * @param name the gene name
  * @param properties the gene properties
  * @param conversionMap the [[ConversionMap]]
  */
case class CustomGeneInfo(override val id: String,
                          override val name: String,
                          override val  properties: Map[String, Class[_]],
                          conversionMap: Map[String, Map[String, Double]]) extends GeneInfo(id, name, properties)

/**
  * It describes the relations between a property and qualities.
  * @param property the property name
  * @param map the relations (values) with the qualities (keys)
  */
case class ConversionMap(property: String,
                         map: Map[String, Double])

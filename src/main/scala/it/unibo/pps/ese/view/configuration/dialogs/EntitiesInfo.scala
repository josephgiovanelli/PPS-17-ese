package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.controller.loader.DefaultGene
import it.unibo.pps.ese.controller.loader.data.AlleleData

case class AnimalBaseInfo(geneLength: Int, alleleLength: Int, reign: String, typology: String)
case class AnimalChromosomeInfo(var structuralChromosome: Map[String, CustomGeneInfo], var regulationChromosome: Map[String, DefaultGeneInfo],
                                var sexualChromosome: Map[String, DefaultGeneInfo])
case class PlantInfo(height: Double, nutritionalValue: Double, hardness: Double, availability: Double)


class GeneInfo(val id: String, val name: String, val properties: Map[String, Class[_]], var alleles: Set[AlleleData] = Set.empty)
case class DefaultGeneInfo(defaultGene: DefaultGene, override val id: String)
  extends GeneInfo(id, defaultGene.name, defaultGene.properties)
case class CustomGeneInfo(override val id: String, override val name: String,override val  properties: Map[String, Class[_]], conversionMap: Map[String, Map[String, Double]]) extends GeneInfo(id, name, properties)
case class ConversionMap(property: String, map: Map[String, Double])
sealed trait EntitiesInfo {

  def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit

  def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit

  def getAnimalInfo(id: String): Option[(AnimalBaseInfo, AnimalChromosomeInfo)]

  def setPlantInfo(id: String, plantInfo: PlantInfo): Unit

  def getPlantInfo(id: String): Option[PlantInfo]
}

object EntitiesInfo {
  private val _instance = new EntitiesInfoImpl()
  def instance() =
    _instance

  class EntitiesInfoImpl() extends EntitiesInfo {
    private var animals: Map[String, (AnimalBaseInfo, AnimalChromosomeInfo)] = Map.empty
    private var plants: Map[String, PlantInfo] = Map.empty


    def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit = {
      val animalChromosomeInfo = if (animals.get(id).isDefined) animals(id)._2
                                 else AnimalChromosomeInfo(Map.empty, Map.empty, Map.empty)
      animals += (id -> (animalBaseInfo, animalChromosomeInfo))
    }

    def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit =
      animals += (id -> (animals(id)._1, animalChromosomeInfo))

    def getAnimalInfo(id: String): Option[(AnimalBaseInfo, AnimalChromosomeInfo)] = animals.get(id)

    def setPlantInfo(id: String, plantInfo: PlantInfo): Unit = plants += (id -> plantInfo)

    def getPlantInfo(id: String): Option[PlantInfo] = plants.get(id)

  }
}







package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.controller.loader.{DefaultGene, RegulationDefaultGenes, SexualDefaultGenes}
import it.unibo.pps.ese.controller.loader.beans.{Allele, Gene, Plant, PropertyInfo}
import it.unibo.pps.ese.controller.loader.data._

case class AnimalBaseInfo(geneLength: Int,
                          alleleLength: Int,
                          typology: String)

case class AnimalChromosomeInfo(var structuralChromosome: Map[String, (CustomGeneInfo, Map[String, AlleleInfo])],
                                var regulationChromosome: Map[String, (DefaultGeneInfo, Map[String, AlleleInfo])],
                                var sexualChromosome: Map[String, (DefaultGeneInfo, Map[String, AlleleInfo])])

case class PlantInfo(height: Double,
                     nutritionalValue: Double,
                     hardness: Double,
                     availability: Double)

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

case class AlleleInfo(gene: String,
                      id: String,
                      dominance: Double,
                      consume: Double,
                      probability: Double,
                      var effect: Map[String, Double])

sealed trait EntitiesInfo {

  def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit

  def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit

  def getAnimalInfo(id: String): Option[(AnimalBaseInfo, AnimalChromosomeInfo)]

  def setPlantInfo(id: String, plantInfo: PlantInfo): Unit

  def getPlantInfo(id: String): Option[PlantInfo]
}

object ChromosomeTypes extends Enumeration {
  val STRUCTURAL, REGULATION, SEXUAL = Value
}

object EntitiesInfo {
  private val _instance = new EntitiesInfoImpl()
  def instance(): EntitiesInfoImpl =
    _instance

  class EntitiesInfoImpl() extends EntitiesInfo {
    private var animals: Map[String, (AnimalBaseInfo, AnimalChromosomeInfo)] = Map.empty
    private var plants: Map[String, PlantInfo] = Map.empty

    private val typologyMap = Map("Carnivorous" -> "C", "Herbivore" -> "H")


    def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit = {
      val animalChromosomeInfo = if (animals.get(id).isDefined) animals(id)._2
                                 else AnimalChromosomeInfo(Map.empty, Map.empty, Map.empty)
      animals += (id -> (animalBaseInfo, animalChromosomeInfo))
    }

    def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit = animals += (id -> (animals(id)._1, animalChromosomeInfo))

    def getAnimalInfo(id: String): Option[(AnimalBaseInfo, AnimalChromosomeInfo)] = animals.get(id)

    def setPlantInfo(id: String, plantInfo: PlantInfo): Unit = plants += (id -> plantInfo)

    def getPlantInfo(id: String): Option[PlantInfo] = plants.get(id)

    def getAnimals: Set[String] = animals.keySet

    def getPlants: Set[String] = plants.keySet

    def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes.Value, customGeneInfo: CustomGeneInfo): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalInfo(id) match {
        case Some((_, chromosomeInfo)) => chromosomeInfo
        case None => throw new IllegalStateException()
      }
      val currentStructuralChromosome = currentAnimalChromosome.structuralChromosome
      val alleles: Map[String, AlleleInfo] = if (currentStructuralChromosome.get(customGeneInfo.name).isDefined) currentStructuralChromosome(customGeneInfo.name)._2 else Map()
      val tuple = (customGeneInfo, alleles)
      currentAnimalChromosome.structuralChromosome += (customGeneInfo.name -> tuple)
    }

    def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes.Value, defaultGeneInfo: DefaultGeneInfo): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalInfo(id) match {
        case Some((_, chromosomeInfo)) => chromosomeInfo
        case None => throw new IllegalStateException()
      }
      var currentDefaultChromosome = chromosomeTypes match {
        case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome
        case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome
      }
      val alleles: Map[String, AlleleInfo] = if (currentDefaultChromosome.get(defaultGeneInfo.name).isDefined) currentDefaultChromosome(defaultGeneInfo.name)._2 else Map()
      currentDefaultChromosome += (defaultGeneInfo.name -> (defaultGeneInfo, alleles))
      chromosomeTypes match {
        case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome = currentDefaultChromosome
        case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome = currentDefaultChromosome
      }
    }

    def setChromosomeAlleles(id: String, chromosomeTypes: ChromosomeTypes.Value, gene: String, alleles: Map[String, AlleleInfo]): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(id) match {
        case Some((_, chromosomeInfo)) => chromosomeInfo
        case None => throw new IllegalStateException()
      }
      chromosomeTypes match {
        case ChromosomeTypes.STRUCTURAL =>
          val structuralGene = currentAnimalChromosome.structuralChromosome(gene)
          currentAnimalChromosome.structuralChromosome += (gene -> (structuralGene._1, structuralGene._2 ++ alleles))
        case ChromosomeTypes.REGULATION =>
          val regulationGene = currentAnimalChromosome.regulationChromosome(gene)
          currentAnimalChromosome.regulationChromosome += (gene -> (regulationGene._1, regulationGene._2 ++ alleles))
        case ChromosomeTypes.SEXUAL =>
          val sexualGene = currentAnimalChromosome.sexualChromosome(gene)
          currentAnimalChromosome.sexualChromosome += (gene -> (sexualGene._1, sexualGene._2 ++ alleles))
      }
    }

    def getSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): SimulationData =
      SimulationData(animalsMapping(animalsEntities), plantsMapping(plantsEntities))


    private def plantsMapping(plantsEntities: Map[String, Int]): Map[PlantData, Int] = {
      val mappedPlants: Map[String, PlantData] = plants.map(plant => plant._1 -> Plant(plant._1, 3, 3, "P", plant._2.height, 0, plant._2.hardness, plant._2.nutritionalValue, plant._2.availability))
      mappedPlants.map(mappedPlant => mappedPlant._2 -> plantsEntities(mappedPlant._1))
    }

    private def animalsMapping(animalsEntities: Map[String, Int]): Map[AnimalData, Int] = {
      val mappedAnimals: Map[String, AnimalData] = animals.map(animal => animal._1 -> AnimalData(animal._1, animal._2._1.geneLength, animal._2._1.alleleLength, "A", typologyMap(animal._2._1.typology), structuralChromosomeMapping(animal._1), regulationChromosomeMapping(animal._1), sexualChromosomeMapping(animal._1)))
      mappedAnimals.map(mappedAnimal => mappedAnimal._2 -> animalsEntities(mappedAnimal._1))
    }

    private def sexualChromosomeMapping(animal: String): Iterable[DefaultGeneData] =
      defaultChromosomeMapping(ChromosomeTypes.SEXUAL, animal)

    private def regulationChromosomeMapping(animal: String): Iterable[DefaultGeneData] =
      defaultChromosomeMapping(ChromosomeTypes.REGULATION, animal)

    private def structuralChromosomeMapping(animal: String): Iterable[CustomGeneData] =
      getAnimalInfo(animal).get._2.structuralChromosome.map(gene => CustomGeneData(Gene(gene._2._1.id, gene._2._1.name, "", propertiesMapping(gene._2._1.conversionMap)), alleleMapping(gene._2._1.id, gene._2._2)))

    private def propertiesMapping(properties: Map[String, Map[String, Double]]): Map[String, PropertyInfo] =
      properties.map(property => property._1 -> PropertyInfo(property._2))

    private def defaultChromosomeMapping(chromosomeTypes: ChromosomeTypes.Value, animal: String): Iterable[DefaultGeneData] = {
      val animalChromosomeInfo = getAnimalInfo(animal).get._2
      var enumerationElements: Set[_ <: DefaultGene] = Set.empty
      val defaultChromosomeInfo: Map[String, (DefaultGeneInfo, Map[String, AlleleInfo])] = chromosomeTypes match {
        case ChromosomeTypes.REGULATION => enumerationElements = RegulationDefaultGenes.elements; animalChromosomeInfo.regulationChromosome
        case ChromosomeTypes.SEXUAL => enumerationElements = SexualDefaultGenes.elements; animalChromosomeInfo.sexualChromosome
      }
      defaultChromosomeInfo.map(gene => DefaultGeneData(enumerationElements.filter(x => x.name.equals(gene._2._1.name)).head, gene._2._1.id, alleleMapping(gene._2._1.id, gene._2._2)))
    }

    private def alleleMapping(gene: String, alleles: Map[String, AlleleInfo]): Iterable[AlleleData] =
      alleles.map(allele => Allele(gene, allele._2.id, allele._2.dominance, allele._2.consume, allele._2.probability, allele._2.effect))
  }
}







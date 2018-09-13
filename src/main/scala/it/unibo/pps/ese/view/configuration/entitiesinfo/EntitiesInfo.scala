package it.unibo.pps.ese.view.configuration.entitiesinfo

import it.unibo.pps.ese.controller.loader.beans.{Allele, Gene, Plant, PropertyInfo}
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.{DefaultGene, RegulationDefaultGenes, SexualDefaultGenes}
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals._
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.plants.PlantInfo


sealed trait EntitiesInfo {

  /*
  Animals
   */

  def getAnimalInfo(id: String): Option[AnimalInfo]

  def getAnimalBaseInfo(id: String): AnimalBaseInfo

  def getAnimalChromosomeInfo(id: String): AnimalChromosomeInfo


  def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit

  def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit

  def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes.Value, customGeneInfo: CustomGeneInfo): Unit

  def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes.Value, defaultGeneInfo: DefaultGeneInfo): Unit

  def setChromosomeAlleles(id: String, chromosomeTypes: ChromosomeTypes.Value, gene: String, alleles: Map[String, AlleleInfo]): Unit


  /*
  Plants
   */

  def getPlantInfo(id: String): Option[PlantInfo]

  def setPlantInfo(id: String, plantInfo: PlantInfo): Unit

  /*
  Simulation
   */

  def getAnimals: Set[String]

  def getPlants: Set[String]

  def getSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): SimulationData

  def loadSimulationData(animalData: Seq[AnimalData], plantData: Seq[PlantData]): Unit

}

object ChromosomeTypes extends Enumeration {
  val STRUCTURAL, REGULATION, SEXUAL = Value
}

object EntitiesInfo {
  private val _instance = new EntitiesInfoImpl()
  def instance(): EntitiesInfoImpl =
    _instance

  class EntitiesInfoImpl() extends EntitiesInfo {
    private var animals: Map[String, AnimalInfo] = Map.empty
    private var plants: Map[String, PlantInfo] = Map.empty

    private val typologyMap = Map(
      "Carnivorous" -> "C",
      "Herbivore" -> "H",
      "C" -> "Carnivorous",
      "H" -> "Herbivore")

    /*
    Animals
     */

    def getAnimalInfo(id: String): Option[AnimalInfo] = animals.get(id)

    def getAnimalBaseInfo(id: String): AnimalBaseInfo = animals.get(id) match {
      case Some(animalInfo) => animalInfo.animalBaseInfo
      case None => throw new IllegalStateException()
    }

    def getAnimalChromosomeInfo(id: String): AnimalChromosomeInfo = animals.get(id) match {
      case Some(animalInfo) => animalInfo.animalChromosomeInfo
      case None => throw new IllegalStateException()
    }

    def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit = {
      val animalChromosomeInfo = if (animals.get(id).isDefined) animals(id).animalChromosomeInfo
      else AnimalChromosomeInfo(Map.empty, Map.empty, Map.empty)
      animals += (id -> AnimalInfo(animalBaseInfo, animalChromosomeInfo))
    }

    def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit =
      animals += (id -> AnimalInfo(animals(id).animalBaseInfo, animalChromosomeInfo))

    def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes.Value, customGeneInfo: CustomGeneInfo): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalChromosomeInfo(id)
      val currentStructuralChromosome = currentAnimalChromosome.structuralChromosome
      val alleles: Map[String, AlleleInfo] = if (currentStructuralChromosome.get(customGeneInfo.name).isDefined) currentStructuralChromosome(customGeneInfo.name).alleles else Map()
      currentAnimalChromosome.structuralChromosome += (customGeneInfo.name -> CustomChromosomeInfo(customGeneInfo, alleles))
    }

    def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes.Value, defaultGeneInfo: DefaultGeneInfo): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalInfo(id) match {
        case Some(animalInfo) => animalInfo.animalChromosomeInfo
        case None => throw new IllegalStateException()
      }
      var currentDefaultChromosome = chromosomeTypes match {
        case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome
        case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome
      }
      val alleles: Map[String, AlleleInfo] = if (currentDefaultChromosome.get(defaultGeneInfo.name).isDefined) currentDefaultChromosome(defaultGeneInfo.name).alleles else Map()
      currentDefaultChromosome += (defaultGeneInfo.name -> DefaultChromosomeInfo(defaultGeneInfo, alleles))
      chromosomeTypes match {
        case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome = currentDefaultChromosome
        case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome = currentDefaultChromosome
      }
    }

    def setChromosomeAlleles(id: String, chromosomeTypes: ChromosomeTypes.Value, gene: String, alleles: Map[String, AlleleInfo]): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalChromosomeInfo(id)

      chromosomeTypes match {
        case ChromosomeTypes.STRUCTURAL =>
          val structuralGene = currentAnimalChromosome.structuralChromosome(gene)
          currentAnimalChromosome.structuralChromosome += (gene -> CustomChromosomeInfo(structuralGene.geneInfo, structuralGene.alleles ++ alleles))
        case ChromosomeTypes.REGULATION =>
          val regulationGene = currentAnimalChromosome.regulationChromosome(gene)
          currentAnimalChromosome.regulationChromosome += (gene -> DefaultChromosomeInfo(regulationGene.geneInfo, regulationGene.alleles ++ alleles))
        case ChromosomeTypes.SEXUAL =>
          val sexualGene = currentAnimalChromosome.sexualChromosome(gene)
          currentAnimalChromosome.sexualChromosome += (gene -> DefaultChromosomeInfo(sexualGene.geneInfo, sexualGene.alleles ++ alleles))
      }
    }

    /*
    Plants
     */

    def getPlantInfo(id: String): Option[PlantInfo] = plants.get(id)

    def setPlantInfo(id: String, plantInfo: PlantInfo): Unit = plants += (id -> plantInfo)

    /*
    Simulation
     */

    def getAnimals: Set[String] = animals.keySet

    def getPlants: Set[String] = plants.keySet


    def getSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): SimulationData =
      SimulationData(animalsMapping(animalsEntities), plantsMapping(plantsEntities))

    def loadSimulationData(animalData: Seq[AnimalData], plantData: Seq[PlantData]): Unit = {
      animals ++= animalsMapping(animalData)
      plants ++= plantsMapping(plantData)
    }


    /*
    Mapping methods EntitiesInfo to SimulationData
     */

    private def plantsMapping(plantsEntities: Map[String, Int]): Map[PlantData, Int] = {
      val mappedPlants: Map[String, PlantData] = plants.map(plant => plant._1 -> Plant(plant._1, 3, 3, "P", plant._2.height, 0, plant._2.hardness, plant._2.nutritionalValue, plant._2.availability))
      mappedPlants.map(mappedPlant => mappedPlant._2 -> plantsEntities(mappedPlant._1))
    }

    private def animalsMapping(animalsEntities: Map[String, Int]): Map[AnimalData, Int] = {
      val mappedAnimals: Map[String, AnimalData] = animals.map(animal => animal._1 -> AnimalData(animal._1, animal._2.animalBaseInfo.geneLength, animal._2.animalBaseInfo.alleleLength, "A", typologyMap(animal._2.animalBaseInfo.typology), structuralChromosomeMapping(animal._1), regulationChromosomeMapping(animal._1), sexualChromosomeMapping(animal._1)))
      mappedAnimals.map(mappedAnimal => mappedAnimal._2 -> animalsEntities(mappedAnimal._1))
    }

    private def sexualChromosomeMapping(animal: String): Iterable[DefaultGeneData] =
      defaultChromosomeMapping(ChromosomeTypes.SEXUAL, animal)

    private def regulationChromosomeMapping(animal: String): Iterable[DefaultGeneData] =
      defaultChromosomeMapping(ChromosomeTypes.REGULATION, animal)

    private def structuralChromosomeMapping(animal: String): Iterable[CustomGeneData] =
      getAnimalInfo(animal).get.animalChromosomeInfo.structuralChromosome.map(gene => CustomGeneData(Gene(gene._2.geneInfo.id, gene._2.geneInfo.name, "", propertiesMapping(gene._2.geneInfo.conversionMap)), alleleMapping(gene._2.geneInfo.id, gene._2.alleles)))

    private def propertiesMapping(properties: Map[String, Map[String, Double]]): Map[String, PropertyInfo] =
      properties.map(property => property._1 -> PropertyInfo(property._2))

    private def defaultChromosomeMapping(chromosomeTypes: ChromosomeTypes.Value, animal: String): Iterable[DefaultGeneData] = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalChromosomeInfo(animal)

      var enumerationElements: Set[_ <: DefaultGene] = Set.empty
      val defaultChromosomeInfo: Map[String, DefaultChromosomeInfo] = chromosomeTypes match {
        case ChromosomeTypes.REGULATION => enumerationElements = RegulationDefaultGenes.elements; currentAnimalChromosome.regulationChromosome
        case ChromosomeTypes.SEXUAL => enumerationElements = SexualDefaultGenes.elements; currentAnimalChromosome.sexualChromosome
      }
      defaultChromosomeInfo.map(gene => DefaultGeneData(enumerationElements.filter(x => x.name.equals(gene._2.geneInfo.name)).head, gene._2.geneInfo.id, alleleMapping(gene._2.geneInfo.id, gene._2.alleles)))
    }

    private def alleleMapping(gene: String, alleles: Map[String, AlleleInfo]): Iterable[AlleleData] =
      alleles.map(allele => Allele(gene, allele._2.id, allele._2.dominance, allele._2.consume, allele._2.probability, allele._2.effect))


    /*
    Mapping methods SimulationData to EntitiesInfo
     */

    private def plantsMapping(plantData: Seq[PlantData]): Map[String, PlantInfo] =
      plantData.map(plant => plant.name -> PlantInfo(plant.height, plant.nutritionalValue, plant.hardness, plant.availability)).toMap

    private def animalsMapping(animalData: Seq[AnimalData]): Map[String, AnimalInfo] =
      animalData.map(animal => animal.name -> AnimalInfo(animalBaseInfoMapping(animal), animalChromosomeInfoMapping(animal))).toMap

    private def animalBaseInfoMapping(animal: AnimalData): AnimalBaseInfo =
      AnimalBaseInfo(animal.geneLength, animal.alleleLength, animal.typology)

    private def animalChromosomeInfoMapping(animal: AnimalData): AnimalChromosomeInfo =
      AnimalChromosomeInfo(structuralChromosomeMapping(animal.structuralChromosome), regulationChromosomeMapping(animal.regulationChromosome), sexualChromosomeMapping(animal.sexualChromosome))

    private def sexualChromosomeMapping(sexualChromosome: Set[DefaultGeneData]): Map[String, DefaultChromosomeInfo] =
      sexualChromosome.map(defaultGene => defaultGene.name -> defaultChromosomeMapping(defaultGene, ChromosomeTypes.SEXUAL)).toMap

    private def regulationChromosomeMapping(regulationChromosome: Set[DefaultGeneData]): Map[String, DefaultChromosomeInfo] =
      regulationChromosome.map(defaultGene => defaultGene.name -> defaultChromosomeMapping(defaultGene, ChromosomeTypes.REGULATION)).toMap

    private def structuralChromosomeMapping(structuralChromosome: Set[CustomGeneData]): Map[String, CustomChromosomeInfo] =
      structuralChromosome.map(customGene => customGene.name -> customChromosomeMapping(customGene)).toMap

    private def defaultChromosomeMapping(defaultGene: DefaultGeneData, chromosomeTypes: ChromosomeTypes.Value): DefaultChromosomeInfo =
      DefaultChromosomeInfo(defaultGeneInfoMapping(defaultGene, chromosomeTypes), allelesMapping(defaultGene.alleles))

    private def customChromosomeMapping(customGene: CustomGeneData): CustomChromosomeInfo =
      CustomChromosomeInfo(customGeneInfoMapping(customGene), allelesMapping(customGene.alleles))

    private def defaultGeneInfoMapping(defaultGene: DefaultGeneData, chromosomeTypes: ChromosomeTypes.Value): DefaultGeneInfo = chromosomeTypes match {
      case ChromosomeTypes.REGULATION => DefaultGeneInfo(RegulationDefaultGenes.elements.filter(gene => gene.name.equals(defaultGene.name)).head, defaultGene.id)
      case ChromosomeTypes.SEXUAL => DefaultGeneInfo(SexualDefaultGenes.elements.filter(gene => gene.name.equals(defaultGene.name)).head, defaultGene.id)
    }

    private def customGeneInfoMapping(customGeneData: CustomGeneData): CustomGeneInfo =
      CustomGeneInfo(customGeneData.id, customGeneData.name, customGeneData.properties, customGeneData.conversionMap)

    private def allelesMapping(alleles: Set[AlleleData]): Map[String, AlleleInfo] =
      alleles.map(allele => allele.id -> alleleMapping(allele)).toMap

    private def alleleMapping(allele: AlleleData): AlleleInfo =
      AlleleInfo(allele.gene, allele.id, allele.dominance, allele.consume, allele.probability, allele.effect)

  }
}







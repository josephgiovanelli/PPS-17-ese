package it.unibo.pps.ese.view.configuration.entitiesinfo

import it.unibo.pps.ese.controller.loader.beans.{Allele, Gene, Plant, PropertyInfo}
import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.CustomGeneData.PartialCustomGeneData
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.PartialDefaultGeneData
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder._
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

  def getSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): CompleteSimulationData

  def loadSimulationData(animalData: Seq[PartialAnimalData], plantData: Seq[PartialPlantData]): Unit
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


    def getSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): CompleteSimulationData =
      SimulationBuilder()
      .addAnimals(animalsMapping(animalsEntities))
      .addPlants(plantsMapping(plantsEntities))
      .buildComplete

    def loadSimulationData(animalData: Seq[PartialAnimalData], plantData: Seq[PartialPlantData]): Unit = {
      animals ++= animalsMapping(animalData)
      plants ++= plantsMapping(plantData)
    }


    /*
    Mapping methods EntitiesInfo to SimulationData
     */

    private def plantsMapping(plantsEntities: Map[String, Int]): Map[PartialPlantData, Int] = {
      val mappedPlants: Map[String, PartialPlantData] = plants.map(
        plant =>
          plant._1 ->
            PlantBuilder()
              .setName(plant._1)
              //TODO why?
              .setGeneLength(3)
              .setAlleleLength(3)
              .setReign("P")
              .setHeight(plant._2.height)
              //TODO why?
              .setAttractiveness(0)
              .setHardness(plant._2.hardness)
              .setNutritionalValue(plant._2.nutritionalValue)
              .setAvailability(plant._2.availability)
              .build
      )
      mappedPlants.map(mappedPlant => mappedPlant._2 -> plantsEntities(mappedPlant._1))
    }

    private def animalsMapping(animalsEntities: Map[String, Int]): Map[PartialAnimalData, Int] = {
      val mappedAnimals: Map[String, PartialAnimalData] = animals.map(animal => {
        animal._1 -> {
          AnimalBuilder()
            .setName(animal._1)
            .setGeneLength(animal._2.animalBaseInfo.geneLength)
            .setAlleleLength(animal._2.animalBaseInfo.alleleLength)
            .setReign("A")
            .setTypology(typologyMap(animal._2.animalBaseInfo.typology))
            .addStructuralChromosome(structuralChromosomeMapping(animal._1))
            .addRegulationChromosome(regulationChromosomeMapping(animal._1))
            .addSexualChromosome(sexualChromosomeMapping(animal._1))
            .build
        }
      }).asInstanceOf[Map[String, PartialAnimalData]]
      //TODO required if CompleteAnimalData is a type
      //.asInstanceOf[Map[String, CompleteAnimalData]]
      mappedAnimals.map(mappedAnimal => mappedAnimal._2 -> animalsEntities(mappedAnimal._1))
        .asInstanceOf[Map[PartialAnimalData, Int]]
    }

    private def sexualChromosomeMapping(animal: String): Iterable[GeneBuilder[_]] =
      defaultChromosomeMapping(ChromosomeTypes.SEXUAL, animal)

    private def regulationChromosomeMapping(animal: String): Iterable[GeneBuilder[_]] =
      defaultChromosomeMapping(ChromosomeTypes.REGULATION, animal)

    private def structuralChromosomeMapping(animal: String): Iterable[GeneBuilder[_]] =
      getAnimalInfo(animal).get.animalChromosomeInfo.structuralChromosome.map(
        gene =>
        GeneBuilder()
          .setId(gene._2.geneInfo.id)
          .setName(gene._2.geneInfo.name)
          .setCustomProperties(propertiesMapping(gene._2.geneInfo.conversionMap))
          .addAlleles(alleleMapping(gene._2.geneInfo.id, gene._2.alleles))
      )

    private def propertiesMapping(properties: Map[String, Map[String, Double]]): Map[String, PropertyInfo] =
      properties.map(property => property._1 -> PropertyInfo(property._2))

    private def defaultChromosomeMapping(chromosomeTypes: ChromosomeTypes.Value, animal: String): Iterable[GeneBuilder[_]] = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalChromosomeInfo(animal)

      var enumerationElements: Set[_ <: DefaultGene] = Set.empty
      val defaultChromosomeInfo: Map[String, DefaultChromosomeInfo] = chromosomeTypes match {
        case ChromosomeTypes.REGULATION => enumerationElements = RegulationDefaultGenes.elements; currentAnimalChromosome.regulationChromosome
        case ChromosomeTypes.SEXUAL => enumerationElements = SexualDefaultGenes.elements; currentAnimalChromosome.sexualChromosome
      }
      defaultChromosomeInfo.map(
        gene =>
        GeneBuilder()
        .setDefaultInfo(enumerationElements.filter(x => x.name.equals(gene._2.geneInfo.name)).head)
        .setId(gene._2.geneInfo.id)
        .addAlleles(alleleMapping(gene._2.geneInfo.id, gene._2.alleles))
      )
    }

    private def alleleMapping(gene: String, alleles: Map[String, AlleleInfo]): Iterable[PartialAlleleData] =
      alleles.map(allele =>
        AlleleBuilder()
          .setId(allele._2.id)
          .setGene(gene)
          .setConsume(allele._2.consume)
          .setDominance(allele._2.dominance)
          .setEffect(allele._2.effect)
          .setProbability(allele._2.probability)
          .build
      )


    /*
    Mapping methods SimulationData to EntitiesInfo
     */

    implicit class DefaultStringOption(op: Option[String]) extends DefaultOption[String](op, "")
    implicit class DefaultIntOption(op: Option[Int]) extends DefaultOption[Int](op, -1)
    implicit class DefaultDoubleOption(op: Option[Double]) extends DefaultOption[Double](op, -1)
    implicit class DefaultMapOption[X, Y](op: Option[Map[X, Y]]) extends DefaultOption[Map[X, Y]](op, Map())
    implicit class DefaultSetOption[X](op: Option[Set[X]]) extends DefaultOption[Set[X]](op, Set())
    implicit class DefaultIterableOption[X](op: Option[Iterable[X]]) extends DefaultOption[Iterable[X]](op, Seq())

    abstract class DefaultOption[T](op: Option[T], defaultValue:T) {
      def getOrDefault: T = {
        op.getOrElse(defaultValue)
      }
    }

    private def plantsMapping(plantData: Seq[PartialPlantData]): Map[String, PlantInfo] =
      plantData.map(plant => plant.getName.getOrDefault -> PlantInfo(plant.getHeight.getOrDefault, plant.getNutritionalValue.getOrDefault, plant.getHardness.getOrDefault, plant.getAvailability.getOrDefault)).toMap

    private def animalsMapping(animalData: Seq[PartialAnimalData]): Map[String, AnimalInfo] =
                                //TODO name must be univocal
      animalData.map(animal => animal.getName.getOrDefault -> AnimalInfo(animalBaseInfoMapping(animal), animalChromosomeInfoMapping(animal))).toMap

    private def animalBaseInfoMapping(animal: PartialAnimalData): AnimalBaseInfo =
      AnimalBaseInfo(animal.getGeneLength.getOrDefault, animal.getAlleleLength.getOrDefault, animal.getTypology.getOrDefault)

    private def animalChromosomeInfoMapping(animal: PartialAnimalData): AnimalChromosomeInfo =
      AnimalChromosomeInfo(structuralChromosomeMapping(animal.getStructuralChromosome.getOrDefault), regulationChromosomeMapping(animal.getRegulationChromosome.getOrDefault), sexualChromosomeMapping(animal.getSexualChromosome.getOrDefault))

    private def sexualChromosomeMapping(sexualChromosome: Iterable[PartialDefaultGeneData]): Map[String, DefaultChromosomeInfo] =
      sexualChromosome.map(defaultGene => defaultGene.getName.getOrDefault -> defaultChromosomeMapping(defaultGene, ChromosomeTypes.SEXUAL)).toMap

    private def regulationChromosomeMapping(regulationChromosome: Iterable[PartialDefaultGeneData]): Map[String, DefaultChromosomeInfo] =
      regulationChromosome.map(defaultGene => defaultGene.getName.getOrDefault -> defaultChromosomeMapping(defaultGene, ChromosomeTypes.REGULATION)).toMap

    private def structuralChromosomeMapping(structuralChromosome: Iterable[PartialCustomGeneData]): Map[String, CustomChromosomeInfo] =
      structuralChromosome.map(customGene => customGene.getName.getOrDefault -> customChromosomeMapping(customGene)).toMap

    private def defaultChromosomeMapping(defaultGene: PartialDefaultGeneData, chromosomeTypes: ChromosomeTypes.Value): DefaultChromosomeInfo =
      DefaultChromosomeInfo(defaultGeneInfoMapping(defaultGene, chromosomeTypes), allelesMapping(defaultGene.getAlleles.getOrDefault.toSet[PartialAlleleData]))//TODO check set covariance

    private def customChromosomeMapping(customGene: PartialCustomGeneData): CustomChromosomeInfo =
      CustomChromosomeInfo(customGeneInfoMapping(customGene), allelesMapping(customGene.getAlleles.getOrDefault.toSet[PartialAlleleData]))

    private def defaultGeneInfoMapping(defaultGene: PartialDefaultGeneData, chromosomeTypes: ChromosomeTypes.Value): DefaultGeneInfo = chromosomeTypes match {
      case ChromosomeTypes.REGULATION => DefaultGeneInfo(RegulationDefaultGenes.elements.filter(gene => gene.name.equals(defaultGene.getName.getOrDefault)).head, defaultGene.getId.getOrDefault)
      case ChromosomeTypes.SEXUAL => DefaultGeneInfo(SexualDefaultGenes.elements.filter(gene => gene.name.equals(defaultGene.getName.getOrDefault)).head, defaultGene.getId.getOrDefault)
    }

    private def customGeneInfoMapping(customGeneData: PartialCustomGeneData): CustomGeneInfo =
      CustomGeneInfo(customGeneData.getId.getOrDefault, customGeneData.getName.getOrDefault,
        customGeneData.getProperties.getOrDefault, customGeneData.getConversionMap.getOrDefault)

    private def allelesMapping(alleles: Set[PartialAlleleData]): Map[String, AlleleInfo] =
      alleles.map(allele => allele.getId.getOrDefault -> alleleMapping(allele)).toMap

    private def alleleMapping(allele: PartialAlleleData): AlleleInfo =
      AlleleInfo(allele.getGene.getOrDefault, allele.getId.getOrDefault, allele.getDominance.getOrDefault, allele.getConsume.getOrDefault, allele.getProbability.getOrDefault, allele.getEffect.getOrDefault)

  }
}







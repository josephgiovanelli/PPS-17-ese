package it.unibo.pps.ese.view.sections.configuration.entitiesinfo

import it.unibo.pps.ese.controller.simulation.loader.beans.PropertyInfo
import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.PartialAnimalData
import it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData.PartialCustomGeneData
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.PartialDefaultGeneData
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.data._
import it.unibo.pps.ese.controller.simulation.loader.data.builder._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.{AnimalBuilder, PlantBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.{CustomGeneBuilder, DefaultGeneBuilder}
import it.unibo.pps.ese.controller.simulation.loader.{DefaultGene, RegulationDefaultGenes, SexualDefaultGenes}
import it.unibo.pps.ese.utils.DefaultValue
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals._
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.plants.PlantInfo

import scala.util.Try

/**
  * The trait that allows to configure an entity.
  */
sealed trait EntitiesInfo {

  /*
  Animals
   */

  /**
    * It gets the animal if exists.
    * @param id the animal identifier
    * @return the animal reference
    */
  def getAnimalInfo(id: String): Option[AnimalInfo]

  /**
    * It gets only the base info of the animal.
    * @param id the animal identifier
    * @return the base info of the animal
    */
  def getAnimalBaseInfo(id: String): AnimalBaseInfo

  /**
    * It gets only the animal chromosome.
    * @param id the animal identifier
    * @return the animal chromosome
    */
  def getAnimalChromosomeInfo(id: String): AnimalChromosomeInfo

  /**
    * It allows to set the animal base info.
    * @param id animal identifier
    * @param animalBaseInfo the animal base info
    */
  def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit

  /**
    * It allows to set the animal chromosome.
    * @param id animal identifier
    * @param animalChromosomeInfo the animal chromosome
    */
  def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit

  /**
    * It allows to set the base info of a gene of a chromosome.
    * @param id animal identifier
    * @param chromosomeTypes the animal chromosome types ([[StructuralChromosome]]
    * @param customGeneInfo the base info of animal chromosome
    */
  def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes, customGeneInfo: CustomGeneInfo): Unit

  /**
    * It allows to set the base info of a gene of a chromosome.
    * @param id animal identifier
    * @param chromosomeTypes the animal chromosome types ([[RegulationChromosome]], [[SexualChromosome]])
    * @param defaultGeneInfo the base info of animal chromosome
    */
  def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes, defaultGeneInfo: DefaultGeneInfo): Unit

  /**
    * It allows to set the alleles of a gene.
    * @param id animal identifier
    * @param chromosomeTypes the animal chromosome types ([[StructuralChromosome]], [[RegulationChromosome]], [[SexualChromosome]])
    * @param gene the gene identifier
    * @param alleles the alleles of the gene
    */
  def setChromosomeAlleles(id: String, chromosomeTypes: ChromosomeTypes, gene: String, alleles: Map[String, AlleleInfo]): Unit

  /*
  Plants
   */

  /**
    * It gets the plant if exists.
    * @param id the plant identifier
    * @return the plant reference
    */
  def getPlantInfo(id: String): Option[PlantInfo]

  /**
    * It allows to set the plant information.
    * @param id the plant identifier
    * @param plantInfo the plant information
    */
  def setPlantInfo(id: String, plantInfo: PlantInfo): Unit

  /*
  Simulation
   */

  /**
    * It allows to delete all entities.
    */
  def deleteAllEntities(): Unit

  /**
    * It allows to delete a plant.
    * @param id the plant identifier
    */
  def deletePlant(id: String): Unit

  /**
    * It allows to delete an animal.
    * @param id the animal identifier
    */
  def deleteAnimal(id: String): Unit

  /**
    * It gets all animal species.
    * @return a set of animal species
    */
  def getAnimals: Set[String]

  /**
    * It gets all plant species.
    * @return a set of plant species
    */
  def getPlants: Set[String]

  /**
    * It create a [[SimulationData]] based on the information inserted
    * @param animalsEntities for each animal species the number of individuals to be created is specified
    * @param plantsEntities for each plant species the number of individuals to be created is specified
    * @return a simulation data that have to be complete to run a simulation
    */
  def getSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): Try[CompleteSimulationData]

  /**
    * It create a [[SimulationData]] based on the information inserted
    * @param animalsEntities for each animal species the number of individuals to be created is specified
    * @param plantsEntities for each plant species the number of individuals to be created is specified
    * @return a simulation data that may be incomplete
    */
  def getPartialSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): PartialSimulationData

  /**
    * It creates the editable information by a [[SimulationData]]
    * @param animalData the animal information to add that may be incomplete, if you want to add something else
    * @param plantData the plant information to add that may be incomplete, if you want to add something else
    */
  def loadSimulationData(animalData: Iterable[PartialAnimalData], plantData: Iterable[PartialPlantData]): Unit
}

/**
  * Enumeration that define the types of chromosome.
  */
trait ChromosomeTypes
object StructuralChromosome extends ChromosomeTypes
object RegulationChromosome extends ChromosomeTypes
object SexualChromosome extends ChromosomeTypes

/**
  * The singleton that allows to access at the information at any time.
  */
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

    override def getAnimalInfo(id: String): Option[AnimalInfo] = animals.get(id)

    override def getAnimalBaseInfo(id: String): AnimalBaseInfo = animals.get(id) match {
      case Some(animalInfo) => animalInfo.animalBaseInfo
      case None => throw new IllegalStateException()
    }

    override def getAnimalChromosomeInfo(id: String): AnimalChromosomeInfo = animals.get(id) match {
      case Some(animalInfo) => animalInfo.animalChromosomeInfo
      case None => throw new IllegalStateException()
    }

    override def setAnimalBaseInfo(id: String, animalBaseInfo: AnimalBaseInfo): Unit = {
      val animalChromosomeInfo = if (animals.get(id).isDefined) animals(id).animalChromosomeInfo
      else AnimalChromosomeInfo(Map.empty, Map.empty, Map.empty)
      animals += (id -> AnimalInfo(animalBaseInfo, animalChromosomeInfo))
    }

    override def setAnimalChromosomeInfo(id: String, animalChromosomeInfo: AnimalChromosomeInfo): Unit =
      animals += (id -> AnimalInfo(animals(id).animalBaseInfo, animalChromosomeInfo))

    override def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes, customGeneInfo: CustomGeneInfo): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalChromosomeInfo(id)
      val currentStructuralChromosome = currentAnimalChromosome.structuralChromosome
      val alleles: Map[String, AlleleInfo] = if (currentStructuralChromosome.get(customGeneInfo.name).isDefined) currentStructuralChromosome(customGeneInfo.name).alleles else Map()
      currentAnimalChromosome.structuralChromosome += (customGeneInfo.name -> CustomChromosomeInfo(customGeneInfo, alleles))
    }

    override def setChromosomeBaseInfo(id: String, chromosomeTypes: ChromosomeTypes, defaultGeneInfo: DefaultGeneInfo): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalInfo(id) match {
        case Some(animalInfo) => animalInfo.animalChromosomeInfo
        case None => throw new IllegalStateException()
      }
      var currentDefaultChromosome = chromosomeTypes match {
        case RegulationChromosome => currentAnimalChromosome.regulationChromosome
        case SexualChromosome => currentAnimalChromosome.sexualChromosome
      }
      val alleles: Map[String, AlleleInfo] = if (currentDefaultChromosome.get(defaultGeneInfo.name).isDefined) currentDefaultChromosome(defaultGeneInfo.name).alleles else Map()
      currentDefaultChromosome += (defaultGeneInfo.name -> DefaultChromosomeInfo(defaultGeneInfo, alleles))
      chromosomeTypes match {
        case RegulationChromosome => currentAnimalChromosome.regulationChromosome = currentDefaultChromosome
        case SexualChromosome => currentAnimalChromosome.sexualChromosome = currentDefaultChromosome
      }
    }

    override def setChromosomeAlleles(id: String, chromosomeTypes: ChromosomeTypes, gene: String, alleles: Map[String, AlleleInfo]): Unit = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalChromosomeInfo(id)

      chromosomeTypes match {
        case StructuralChromosome =>
          val structuralGene = currentAnimalChromosome.structuralChromosome(gene)
          currentAnimalChromosome.structuralChromosome += (gene -> CustomChromosomeInfo(structuralGene.geneInfo, structuralGene.alleles ++ alleles))
        case RegulationChromosome =>
          val regulationGene = currentAnimalChromosome.regulationChromosome(gene)
          currentAnimalChromosome.regulationChromosome += (gene -> DefaultChromosomeInfo(regulationGene.geneInfo, regulationGene.alleles ++ alleles))
        case SexualChromosome =>
          val sexualGene = currentAnimalChromosome.sexualChromosome(gene)
          currentAnimalChromosome.sexualChromosome += (gene -> DefaultChromosomeInfo(sexualGene.geneInfo, sexualGene.alleles ++ alleles))
      }
    }

    /*
    Plants
     */

    override def getPlantInfo(id: String): Option[PlantInfo] = plants.get(id)

    override def setPlantInfo(id: String, plantInfo: PlantInfo): Unit = plants += (id -> plantInfo)

    /*
    Simulation
     */

    override def deleteAllEntities(): Unit = {
      plants = Map.empty
      animals = Map.empty
    }

    override def deletePlant(id: String): Unit = plants -= id

    override def deleteAnimal(id: String): Unit = animals -= id

    override def getAnimals: Set[String] = animals.keySet

    override def getPlants: Set[String] = plants.keySet


    override def getSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): Try[CompleteSimulationData] =
      SimulationBuilder()
      .addAnimals(animalsMapping(animalsEntities))
      .addPlants(plantsMapping(plantsEntities))
      .tryCompleteBuild

    override def getPartialSimulationData(animalsEntities: Map[String, Int], plantsEntities: Map[String, Int]): PartialSimulationData =
      SimulationBuilder()
        .addAnimals(animalsMapping(animalsEntities))
        .addPlants(plantsMapping(plantsEntities))
        .build()

    override def loadSimulationData(animalData: Iterable[PartialAnimalData], plantData: Iterable[PartialPlantData]): Unit = {
      animals ++= animalsMapping(animalData)
      plants ++= plantsMapping(plantData)
    }


    /*
    Utilities methods that map EntitiesInfo to SimulationData
     */

    private def plantsMapping(plantsEntities: Map[String, Int]): Map[PlantBuilder[_], Int] = {
      val mappedPlants = plants.map(
        plant =>
          plant._1 ->
            PlantBuilder()
              .setName(plant._1)
              .setGeneLength(3)
              .setAlleleLength(3)
              .setReign("P")
              .setHeight(plant._2.height)
              .setHardness(plant._2.hardness)
              .setNutritionalValue(plant._2.nutritionalValue)
      )
      mappedPlants.map(mappedPlant => mappedPlant._2 -> plantsEntities(mappedPlant._1))
    }

    private def animalsMapping(animalsEntities: Map[String, Int]): Map[AnimalBuilder[_], Int] = {
      val mappedAnimals = animals.map(animal => {
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
        }
      })
      mappedAnimals.map(mappedAnimal => mappedAnimal._2 -> animalsEntities(mappedAnimal._1))
    }

    private def sexualChromosomeMapping(animal: String): Iterable[DefaultGeneBuilder[_]] =
      defaultChromosomeMapping(SexualChromosome, animal)

    private def regulationChromosomeMapping(animal: String): Iterable[DefaultGeneBuilder[_]] =
      defaultChromosomeMapping(RegulationChromosome, animal)

    private def structuralChromosomeMapping(animal: String): Iterable[CustomGeneBuilder[_]] =
      getAnimalInfo(animal).get.animalChromosomeInfo.structuralChromosome.map(
        gene =>
          CustomGeneBuilder()
          .setId(gene._2.geneInfo.id)
          .setName(gene._2.geneInfo.name)
          .setCustomProperties(propertiesMapping(gene._2.geneInfo.conversionMap))
          .addAlleles(alleleMapping(gene._2.geneInfo.id, gene._2.alleles))
      )

    private def propertiesMapping(properties: Map[String, Map[String, Double]]): Map[String, PropertyInfo] =
      properties.map(property => property._1 -> PropertyInfo(property._2))

    private def defaultChromosomeMapping(chromosomeTypes: ChromosomeTypes, animal: String): Iterable[DefaultGeneBuilder[_]] = {
      val currentAnimalChromosome: AnimalChromosomeInfo = getAnimalChromosomeInfo(animal)

      var enumerationElements: Set[_ <: DefaultGene] = Set.empty
      val defaultChromosomeInfo: Map[String, DefaultChromosomeInfo] = chromosomeTypes match {
        case RegulationChromosome => enumerationElements = RegulationDefaultGenes.elements; currentAnimalChromosome.regulationChromosome
        case SexualChromosome => enumerationElements = SexualDefaultGenes.elements; currentAnimalChromosome.sexualChromosome
      }
      defaultChromosomeInfo.map(
        gene =>
          DefaultGeneBuilder()
        .setDefaultInfo(enumerationElements.filter(x => x.name.equals(gene._2.geneInfo.name)).head)
        .setId(gene._2.geneInfo.id)
        .addAlleles(alleleMapping(gene._2.geneInfo.id, gene._2.alleles))
      )
    }

    private def alleleMapping(gene: String, alleles: Map[String, AlleleInfo]): Iterable[AlleleBuilder[_]] =
      alleles.map(allele =>
        AlleleBuilder()
          .setId(allele._2.id)
          .setGene(gene)
          .setConsume(allele._2.consume)
          .setDominance(allele._2.dominance)
          .setEffect(allele._2.effect)
          .setProbability(allele._2.probability)
      )


    /*
    Utilities methods that map SimulationData to EntitiesInfo
     */


    import it.unibo.pps.ese.utils.DefaultGet._

    implicit val int: DefaultValue[Int] = DefaultValue(Integer.MIN_VALUE)
    implicit val double: DefaultValue[Double] = DefaultValue(Double.MinValue)
    implicit val string: DefaultValue[String] = DefaultValue("")
    implicit def iterable[X]: DefaultValue[Iterable[X]] = DefaultValue(Iterable[X]())
    implicit def map[X, Y]: DefaultValue[Map[X, Y]] = DefaultValue(Map[X, Y]())
    implicit def set[X]: DefaultValue[Set[X]] = DefaultValue(Set[X]())

    private def plantsMapping(plantData: Iterable[PartialPlantData]): Map[String, PlantInfo] =
      plantData.map(plant => plant.name -> PlantInfo(plant.getHeight.getOrDefault, plant.getNutritionalValue.getOrDefault, plant.getHardness.getOrDefault)).toMap

    private def animalsMapping(animalData: Iterable[PartialAnimalData]): Map[String, AnimalInfo] =
      animalData.map(animal => animal.name -> AnimalInfo(animalBaseInfoMapping(animal), animalChromosomeInfoMapping(animal))).toMap

    private def animalBaseInfoMapping(animal: PartialAnimalData): AnimalBaseInfo =
      AnimalBaseInfo(animal.getGeneLength.getOrDefault, animal.getAlleleLength.getOrDefault, typologyMap(animal.getTypology.getOrDefault))

    private def animalChromosomeInfoMapping(animal: PartialAnimalData): AnimalChromosomeInfo =
      AnimalChromosomeInfo(structuralChromosomeMapping(animal.getStructuralChromosome.getOrDefault), regulationChromosomeMapping(animal.getRegulationChromosome.getOrDefault), sexualChromosomeMapping(animal.getSexualChromosome.getOrDefault))

    private def sexualChromosomeMapping(sexualChromosome: Iterable[PartialDefaultGeneData]): Map[String, DefaultChromosomeInfo] =
      sexualChromosome.map(defaultGene => defaultGene.name -> defaultChromosomeMapping(defaultGene, SexualChromosome)).toMap

    private def regulationChromosomeMapping(regulationChromosome: Iterable[PartialDefaultGeneData]): Map[String, DefaultChromosomeInfo] =
      regulationChromosome.map(defaultGene => defaultGene.name -> defaultChromosomeMapping(defaultGene, RegulationChromosome)).toMap

    private def structuralChromosomeMapping(structuralChromosome: Iterable[PartialCustomGeneData]): Map[String, CustomChromosomeInfo] =
      structuralChromosome.map(customGene => customGene.name -> customChromosomeMapping(customGene)).toMap

    private def defaultChromosomeMapping(defaultGene: PartialDefaultGeneData, chromosomeTypes: ChromosomeTypes): DefaultChromosomeInfo =
      DefaultChromosomeInfo(defaultGeneInfoMapping(defaultGene, chromosomeTypes), allelesMapping(defaultGene.getAlleles.getOrDefault.toSet))


    private def customChromosomeMapping(customGene: PartialCustomGeneData): CustomChromosomeInfo =
      CustomChromosomeInfo(customGeneInfoMapping(customGene), allelesMapping(customGene.getAlleles.getOrDefault.toSet[PartialAlleleData]))

    private def defaultGeneInfoMapping(defaultGene: PartialDefaultGeneData, chromosomeTypes: ChromosomeTypes): DefaultGeneInfo = chromosomeTypes match {
      case RegulationChromosome => DefaultGeneInfo(RegulationDefaultGenes.elements.filter(gene => gene.name.equals(defaultGene.name)).head, defaultGene.getId.getOrDefault)
      case SexualChromosome => DefaultGeneInfo(SexualDefaultGenes.elements.filter(gene => gene.name.equals(defaultGene.name)).head, defaultGene.getId.getOrDefault)
    }

    private def customGeneInfoMapping(customGeneData: PartialCustomGeneData): CustomGeneInfo =
      CustomGeneInfo(customGeneData.getId.getOrDefault, customGeneData.name,
        customGeneData.getProperties.getOrDefault, customGeneData.getConversionMap.getOrDefault)

    private def allelesMapping[X <: PartialAlleleData](alleles: Set[X]): Map[String, AlleleInfo] =
      alleles.map(allele => allele.id -> alleleMapping(allele)).toMap

    private def alleleMapping(allele: PartialAlleleData): AlleleInfo =
      AlleleInfo(allele.getGene.getOrDefault, allele.id, allele.getDominance.getOrDefault, allele.getConsume.getOrDefault, allele.getProbability.getOrDefault, allele.getEffect.getOrDefault)

  }
}







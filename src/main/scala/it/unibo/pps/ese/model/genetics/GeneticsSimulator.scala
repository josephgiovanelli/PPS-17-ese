package it.unibo.pps.ese.model.genetics

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.simulation.loader.data.CompletePlantData
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.model.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.model.genetics.dna.{AnimalGenome, BasicGene, GeneWithAllelicForms, IdentifierGene, MGene}
import it.unibo.pps.ese.model.genetics.dnaexpression.{AlleleInfo, AllelicGeneStats, BasicGeneStats, GeneStats}
import it.unibo.pps.ese.model.genetics.entities.{Animal, AnimalInfo, Carnivorous, Herbivore, Plant, PlantInfo, QualityType}
import it.unibo.pps.ese.model.genetics.generators.{PlantGenerator, SpeciesUtilities}
import it.unibo.pps.ese.model.genetics.generators.data.InputDataAdapter._

/**
  * A trait following Facade Pattern that exhibits all the genetic's functionalitis
  */
trait GeneticsSimulator {
  /**
    * To start a new simulation given a [[CompleteSimulationData]]
    * @param simulationData
    *                       The [[CompleteSimulationData]] to initialize the simulation
    * @return
    *         A [[InitializedSimulation]]
    */
  def beginSimulation(simulationData:CompleteSimulationData):InitializedSimulation

  /**
    * To restore a previously saved simulation
    * @param savedData
    * @return
    *         The restored [[InitializedSimulation]]
    */
  def restoreSimulation(savedData: SavedData):InitializedSimulation
  def obtainDataToSave:SavedData

  /**
    * To get the list of all animal species
    * @return
    *         Animal species list
    */
  def speciesList:Seq[String]
  /**
    * To get the list of all plant species
    * @return
    *         Plant Species list
    */
  def plantSpeciesList:Seq[String]

  /**
    * To obtain a new plant of the given species
    * @param species
    *                The string that identify the plant species
    * @return
    *         A [[PlantInfo]] that describe the created plant
    */
  def newPlant(species: String):PlantInfo
  /**
    * To obtain a new animal of the given species
    * @param species
    *                The string that identify the animal species
    * @return
    *         A [[AnimalInfo]] that describe the created animal
    */
  def newAnimal(species:String):AnimalInfo

  /**
    * To obtain all the mutant alleles of a given species
    *
    * @param species
    * The string that identify the animal species
    * @param gene
    *     The [[MGene]] for which you want to get the mutant alleles
    * @return
    *         The list of mutant forms
    */
  def obtainMutantAlleles(species:String,gene:MGene):Seq[MGene]

  /**
    * To add a new animal species at runtime
    * @param animalData
    *                   The [[CompleteAnimalData]] to create an animal species
    * @param num
    *            The desidered number of animal of the new species
    * @return
    *         A list of [[it.unibo.pps.ese.model.genetics.entities.AnimalInfo]]
    */
  def addNewAnimalSpecies(animalData:CompleteAnimalData, num:Int):Seq[AnimalInfo]
  /**
    * To add a new plant species at runtime
    * @param plantData
    *                   The [[CompleteAnimalData]] to create an animal species
    * @param num
    *            The desidered number of animal of the new species
    * @return
    *         A list of [[it.unibo.pps.ese.model.genetics.entities.PlantInfo]]
    */
  def addNewPlantSpecies(plantData:CompletePlantData,num:Int):Seq[PlantInfo]

  /**
    * To obtain the [[AnimalInfo]] given the species and the genome of the animal.
    * It translate the dna in information about the animal
    * @param species
    *               The species of the given animal
    * @param genome
    *               The [[AnimalGenome]] of the animal
    * @return
    */
  def getAnimalInfoByGenome(species:String,genome: AnimalGenome):AnimalInfo

  /**
    * To get the mutation list that just appearead in the new animal
    * @param species
    *   The string that identify the animal species
    * @param genome
    *          The genome of the new animal
    * @return
    */
  def checkNewMutation(species:String,genome: AnimalGenome):Seq[MGene]

  /**
    * To retrieve all the information about a given gene
    * @param geneM
    *              The [[MGene]] for which obtain the information
    * @param animalInfo
    *                   The animalInfo of the gene which belongs the gene
    * @return
    *         The [[GeneStats]] of the gene
    */
  def getGeneStats(geneM:MGene, animalInfo: AnimalInfo):GeneStats
}

case class SavedData(simulationData: CompleteSimulationData,notAppearedMutations:Map[String,Seq[AlleleInfo]])

object GeneticsSimulator extends GeneticsSimulator{
  var simulationData:Option[CompleteSimulationData] = None
  private[this] var started:Boolean=false
  private var speciesSetup:Map[String,SpeciesUtilities] = Map()
  private var plantSetup:Map[String,PlantInfo] = Map()
  override def beginSimulation(simulationData: CompleteSimulationData): InitializedSimulation = {
    _checkState()
    this.simulationData = Some(simulationData)
    val initializedSimulation = InitializedSimulation(simulationData)
    setupSimulation(initializedSimulation)
  }

  override def restoreSimulation(savedData: SavedData):InitializedSimulation = {
    _checkState()
    this.simulationData = Some(savedData.simulationData)
    val initializedSimulation = InitializedSimulation(savedData.simulationData,savedData.notAppearedMutations)
    setupSimulation(initializedSimulation)
  }

  private[this] def _checkState():Unit = {
//    if (started) throw new IllegalStateException() else started = true
  }
  private[this] def setupSimulation(initializedSimulation: InitializedSimulation):InitializedSimulation = {
    speciesSetup = initializedSimulation.initialSetup
    plantSetup = initializedSimulation.getAllPlant.map({case (k,v) => (k,v.head)})
    initializedSimulation
  }
  override def speciesList: Seq[String] = speciesSetup.keySet.toSeq

  override def newAnimal(species: String): AnimalInfo = speciesSetup(species).generateAnimal

  override def obtainMutantAlleles(species: String,gene:MGene): Seq[MGene] = speciesSetup(species)
                                                                        .obtainMutantAlleles(gene)


  override def plantSpeciesList: Seq[String] = plantSetup.keySet.toSeq

  override def newPlant(species: String): PlantInfo = plantSetup(species)

  override def addNewAnimalSpecies(animalData: CompleteAnimalData, num: Int): Seq[AnimalInfo] = {
    val name = animalData.name
    val newSetup = SpeciesUtilities(animalData)
    speciesSetup = speciesSetup + (name->newSetup)
    newSetup.generateNumberOfAnimal(num)
  }

  override def addNewPlantSpecies(plantData: CompletePlantData, num: Int): Seq[PlantInfo] = {
    plantSetup  = plantSetup + (plantData.name -> PlantGenerator.createPlantInfoByPlantData(plantData))
    PlantGenerator.createNumberOfPlants(num,plantData)
  }

  override def getAnimalInfoByGenome(species:String,genome: AnimalGenome): AnimalInfo = {
    speciesSetup(species).translateGenome(genome)
  }

  override def getGeneStats(geneM: MGene, animalInfo: AnimalInfo): GeneStats = {

    geneM match {
      case GeneWithAllelicForms(g,a,t) =>
        val speciesUtilities = speciesSetup(animalInfo.species.name)
        val allelicGene = GeneWithAllelicForms(g,a,t)
        val allelicBehaviour = speciesUtilities
          .getAllelicBehaviorOfGene(allelicGene)
        val dominance  = allelicBehaviour.dominanceLevel
        val prob:Double =  speciesUtilities.getProbabilityOfGene(allelicGene)
        val isActive:Boolean = animalInfo.activeAlleles
          .exists(a=>a.geneSeq== allelicGene.geneId && a.allelicSeq == allelicGene.alleleCode)
        val featuresValues =  allelicBehaviour.featuresBehaviour.map(e=>(e._1.name,e._2))
        val affectedQ:Seq[QualityType] = speciesUtilities.getFeaturesOfGene(allelicGene)

        AllelicGeneStats(
          gene = allelicGene,
          dominanceLevel = dominance,
          probability = prob,
          active = isActive,
          affectedQualities = affectedQ,
          features= featuresValues
        )
      case BasicGene(g,t) if t == IdentifierGene => BasicGeneStats(
        gene = geneM,
        identifiedThing = findIdentifiedThings(geneM)
      )
    }
  }

  private[this] def findIdentifiedThings(gene:MGene):String = {
    val herbivoreSeq:Seq[ProteinoGenicAmminoacid] = Herbivore.geneId.completeCode
    val carnivorousSeq:Seq[ProteinoGenicAmminoacid] = Carnivorous.geneId.completeCode
    val animalSeq:Seq[ProteinoGenicAmminoacid] = Animal.geneId.completeCode
    val plantSeq:Seq[ProteinoGenicAmminoacid] = Plant.geneId.completeCode

    gene.completeCode match {
      case `herbivoreSeq` => Herbivore.toString
      case `carnivorousSeq` => Carnivorous.toString
      case `animalSeq` => Animal.toString
      case `plantSeq` => Plant.toString
      case _ => "Species"
    }
  }

  override def checkNewMutation(species:String,genome: AnimalGenome): Seq[MGene] = {
    speciesSetup(species).checkNewApparitions(
      genome.firstGenomeSequence.values.flatMap(c=>c.geneList).toList++
      genome.secondGenomeSequence.values.flatMap(c=>c.geneList).toList++
      genome.firstSexualChromosome.geneList++
      genome.secondSexualChromosome.geneList
    )
  }

  override def obtainDataToSave: SavedData =
    SavedData(
      this.simulationData.getOrElse(throw new IllegalStateException()),
      this.speciesSetup.map{case (k,v)=>
        k -> v.obtainNotAppearedMutation
      }
    )
}

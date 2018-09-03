package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.data.{AnimalData, PlantData, SimulationData}
import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.genetics.dna.{AnimalGenome, BasicGene, GeneWithAllelicForms, IdentifierGene, MGene}
import it.unibo.pps.ese.genetics.dnaexpression.{AllelicGeneStats, BasicGeneStats, GeneStats}
import it.unibo.pps.ese.genetics.entities.{Animal, AnimalInfo, Carnivorous, Herbivore, Plant, PlantInfo, QualityType}
import it.unibo.pps.ese.genetics.generators.{PlantGenerator, SpeciesUtilities}
import it.unibo.pps.ese.genetics.generators.data.InputDataAdapter._

trait GeneticsSimulator {
  def beginSimulation(simulationData:SimulationData):InitializedSimulation
  def speciesList:Seq[String]
  def plantSpeciesList:Seq[String]
  def newPlant(species: String):PlantInfo
  def newAnimal(species:String):AnimalInfo
  def obtainMutantAlleles(species:String,gene:MGene):Seq[MGene]
  def addNewAnimalSpecies(animalData:AnimalData,num:Int):Seq[AnimalInfo]
  def addNewPlantSpecies(plantData:PlantData,num:Int):Seq[PlantInfo]
  def getAnimalInfoByGenome(species:String,genome: AnimalGenome):AnimalInfo
  def getGeneStats(geneM:MGene, animalInfo: AnimalInfo):GeneStats
}
object GeneticsSimulator extends GeneticsSimulator{
  private[this] var started:Boolean=false
  private var speciesSetup:Map[String,SpeciesUtilities] = Map()
  private var plantSetup:Map[String,PlantInfo] = Map()
  override def beginSimulation(simulationData: SimulationData): InitializedSimulation
  = {
    _checkState()
    val initializedSimulation = InitializedSimulation(simulationData)
    speciesSetup = initializedSimulation.initialSetup
    plantSetup = initializedSimulation.getAllPlant.map({case (k,v) => (k,v.head)})
    initializedSimulation
  }
  private[this] def _checkState():Unit = {
//    if (started) throw new IllegalStateException() else started = true
  }
  override def speciesList: Seq[String] = speciesSetup.keySet.toSeq

  override def newAnimal(species: String): AnimalInfo = speciesSetup(species).generateAnimal

  override def obtainMutantAlleles(species: String,gene:MGene): Seq[MGene] = speciesSetup(species)
                                                                        .obtainMutantAlleles(gene)


  override def plantSpeciesList: Seq[String] = plantSetup.keySet.toSeq

  override def newPlant(species: String): PlantInfo = plantSetup(species)

  override def addNewAnimalSpecies(animalData: AnimalData, num: Int): Seq[AnimalInfo] = {
    val name = animalData.name
    val newSetup = SpeciesUtilities(animalData)
    speciesSetup = speciesSetup + (name->newSetup)
    newSetup.generateNumberOfAnimal(num)
  }

  override def addNewPlantSpecies(plantData: PlantData, num: Int): Seq[PlantInfo] = {
    plantSetup  = plantSetup + (plantData.name -> PlantGenerator.createPlantInfoByPlantData(plantData))
    PlantGenerator.createNumberOfPlants(num,plantData)
  }

  override def getAnimalInfoByGenome(species:String,genome: AnimalGenome): AnimalInfo = {
    speciesSetup(species).translateGenome(genome)
  }

  override def getGeneStats(geneM: MGene, animalInfo: AnimalInfo): GeneStats = {

    geneM match {
      case GeneWithAllelicForms(g,a,t) => {
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
      }
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
}

package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.data.{AnimalData, PlantData, SimulationData}
import InputDataAdapter._
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
    if (started) throw new IllegalStateException() else started = true
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
}

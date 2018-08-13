package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.data.SimulationData

trait GeneticsSimulator {
  def beginSimulation(simulationData:SimulationData):InitializedSimulation
  def speciesList:Seq[String]
  def plantSpeciesList:Seq[String]
  def newPlant(species: String):PlantInfo
  def newAnimal(species:String):AnimalInfo
  def mutantAlleleOfSpecies(species:String):Seq[AllelicBehaviour]
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

  override def mutantAlleleOfSpecies(species: String): Seq[AllelicBehaviour] = speciesSetup(species).obtainMutantAlleles

  override def plantSpeciesList: Seq[String] = plantSetup.keySet.toSeq

  override def newPlant(species: String): PlantInfo = plantSetup(species)
}

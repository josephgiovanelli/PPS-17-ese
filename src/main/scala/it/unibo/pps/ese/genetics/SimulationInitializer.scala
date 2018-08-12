package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.data.{PlantData, SimulationData}

sealed trait SimulationInitializer{
  def getAllAnimals:Map[String,Seq[AnimalInfo]]
  def getAllPlant:Map[String,Seq[PlantData]]
}
object SimulationInitializer {
  def apply(simulationData: SimulationData): SimulationInitializer = SimulationInitializerImpl(simulationData)
  private[this] case class SimulationInitializerImpl(simulationData: SimulationData) extends SimulationInitializer{
    private val speciesSetup:Map[String,SpeciesUtilities] = simulationData
      .animals
      .keys
      .map(e=>
        (e.name,
          SpeciesUtilities(InputDataAdapter.translateAnimalData(e))
          ))
      .toMap

    override def getAllAnimals: Map[String, Seq[AnimalInfo]] = {
      simulationData.animals.map(e=>(e._1.name,e._2)).map(c=>{
        var seq:Seq[AnimalInfo] = ( 0 to c._2 by 1).map(i=> {
          val setup: SpeciesUtilities = speciesSetup(c._1)
          val animalGenome = setup.generateAnimalGenome
          setup.translateGenome(animalGenome)
        })
        (c._1,seq)
      })
    }

    override def getAllPlant: Map[String, Seq[PlantInfo]] = {
      simulationData.plants.map(c=>{
        var seq:Seq[PlantInfo] = ( 0 to c._2 by 1).map(i=> {
          PlantGenerator.createPlantInfoByPlantData(c._1)
        })
        (c._1.name,seq)
      })
    }
  }
}

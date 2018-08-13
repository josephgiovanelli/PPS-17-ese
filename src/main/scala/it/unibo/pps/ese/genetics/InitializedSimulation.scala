package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.data.{PlantData, SimulationData}

sealed trait InitializedSimulation{
  private [genetics] def initialSetup:Map[String,SpeciesUtilities]
  def getAllAnimals:Map[String,Seq[AnimalInfo]]
  def getAllPlant:Map[String,Seq[PlantInfo]]
}

object InitializedSimulation {
  def apply(simulationData: SimulationData): InitializedSimulation = InitializedSimulationImpl(simulationData)
  private[this] case class InitializedSimulationImpl(simulationData: SimulationData) extends InitializedSimulation{
    val speciesSetup:Map[String,SpeciesUtilities] = buildSpeciesSetups(simulationData)

    override private[genetics] def initialSetup = speciesSetup
    override def getAllAnimals: Map[String, Seq[AnimalInfo]] = {
      simulationData.animals.map(e=>(e._1.name,e._2)).map(c=>{
        var seq:Seq[AnimalInfo] = ( 1 to c._2 by 1).map(i=> {
          val setup: SpeciesUtilities = speciesSetup(c._1)
          val animalGenome = setup.generateAnimalGenome
          setup.translateGenome(animalGenome)
        })
        (c._1,seq)
      })
    }

    override def getAllPlant: Map[String, Seq[PlantInfo]] = {
      simulationData.plants.map(c=>{
        var seq:Seq[PlantInfo] = ( 1 to c._2 by 1).map(i=> {
          PlantGenerator.createPlantInfoByPlantData(c._1)
        })
        (c._1.name,seq)
      })
    }
  }
  private[this] def buildSpeciesSetups(simulationData: SimulationData):Map[String,SpeciesUtilities] = {
    simulationData
      .animals
      .keys
      .map(e=>
        (e.name,
          SpeciesUtilities(InputDataAdapter.translateAnimalData(e))
        ))
      .toMap
  }
}

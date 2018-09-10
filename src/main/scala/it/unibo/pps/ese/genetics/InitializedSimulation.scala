package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.data.SimulationData
import it.unibo.pps.ese.genetics.dnaexpression.AlleleInfo
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, PlantInfo}
import it.unibo.pps.ese.genetics.generators.data.InputDataAdapter
import it.unibo.pps.ese.genetics.generators.{PlantGenerator, SpeciesUtilities}

sealed trait InitializedSimulation{
  private [genetics] def initialSetup:Map[String,SpeciesUtilities]
  def getAllAnimals:Map[String,Seq[AnimalInfo]]
  def getAllPlant:Map[String,Seq[PlantInfo]]
}

object InitializedSimulation {
  def apply(simulationData: SimulationData): InitializedSimulation = InitializedSimulationImpl(simulationData)
  def apply(simulationData: SimulationData,notAppearedMutations:Map[String,Seq[AlleleInfo]]): InitializedSimulation = {
    val initializedSimulation = InitializedSimulationImpl(simulationData)
    notAppearedMutations.foreach{case(k,v) =>
        initializedSimulation.initialSetup(k).restoreOldNotAppearedAlleles(v)
    }
    initializedSimulation
  }

  private[this] case class InitializedSimulationImpl(simulationData: SimulationData) extends InitializedSimulation{
    val speciesSetup:Map[String,SpeciesUtilities] = buildSpeciesSetups(simulationData)

    override private[genetics] def initialSetup = speciesSetup

    override def getAllAnimals: Map[String, Seq[AnimalInfo]] = {
      simulationData.animals.map(e=>(e._1.name,e._2)).map(c=>{
        val seq:Seq[AnimalInfo] = speciesSetup(c._1).generateNumberOfAnimal(c._2)
        (c._1,seq)
      })
    }

    override def getAllPlant: Map[String, Seq[PlantInfo]] = {
      simulationData.plants.map(c=>{
        val seq:Seq[PlantInfo] = ( 1 to c._2 by 1).map(i=> {
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

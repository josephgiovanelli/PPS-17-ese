package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}


trait SimulationData [_ <: PartialAnimalData] {
  def animals: Map[CompleteAnimalData, Int]
  def plants: Map[PlantData, Int]
}

object SimulationData {
  type PartialSimulationData = SimulationData [_ <: PartialAnimalData]
  type CompleteSimulationData = SimulationData [_ <: CompleteAnimalData]
  def apply(animals: Map[CompleteAnimalData, Int], plants: Map[PlantData, Int]): SimulationData[CompleteAnimalData] =
    SimulationDataImpl(animals, plants)
  def testBuild(animals: Map[CompleteAnimalData, Int], plants: Map[PlantData, Int]): SimulationData[CompleteAnimalData] =
    SimulationDataImpl(animals, plants)
  def ttt(animals: Map[CompleteAnimalData, Int]) = {}
  case class SimulationDataImpl[C <: PartialCustomGeneData, D <: PartialDefaultGeneData](animals: Map[CompleteAnimalData, Int], plants: Map[PlantData, Int]) extends SimulationData[CompleteAnimalData]
}
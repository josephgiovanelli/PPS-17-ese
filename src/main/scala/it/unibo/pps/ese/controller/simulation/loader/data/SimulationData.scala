package it.unibo.pps.ese.controller.simulation.loader.data

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}

trait SimulationData[A <: PartialAnimalData, P <: PartialPlantData] {
  def getAnimals: Option[Iterable[(A, Int)]]
  def getPlants: Option[Iterable[(P, Int)]]
}

trait FullSimulationData[A <: PartialAnimalData, P <: PartialPlantData] extends SimulationData[A, P] {
  def animals: Map[A, Int] = getAnimals.getOrElse(throw new IllegalStateException()).toMap
  def plants: Map[P, Int] = getPlants.getOrElse(throw new IllegalStateException()).toMap
}

object SimulationData {
  type PartialSimulationData = SimulationData [_ <: PartialAnimalData, _ <: PartialPlantData]
  trait CompleteSimulationData extends FullSimulationData [CompleteAnimalData, CompletePlantData]
}
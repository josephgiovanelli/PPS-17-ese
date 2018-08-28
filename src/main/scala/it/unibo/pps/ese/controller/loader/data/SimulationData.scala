package it.unibo.pps.ese.controller.loader.data

trait SimulationData {
  def animals: Map[AnimalData, Int]
  def plants: Map[PlantData, Int]
}

object SimulationData {
  def apply(animals: Map[AnimalData, Int], plants: Map[PlantData, Int]) : SimulationData = SimulationDataImpl(animals, plants)
  case class SimulationDataImpl(animals: Map[AnimalData, Int], plants: Map[PlantData, Int]) extends SimulationData
}
package it.unibo.pps.ese.controller.simulation.loader.data

import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}

/**Trait that defines simulation's data. All non-mandatory fields can be not filled, so are represented by an option
  *
  * @tparam A Internal animals type(completion level)
  * @tparam P Internal plants type(completion level)
  */
trait SimulationData[+A <: PartialAnimalData, +P <: PartialPlantData] {
  /**Simulation's animal with their quantity*/
  val getAnimals: Option[Iterable[(A, Int)]]
  /**Simulation's plants with their quantity*/
  val getPlants: Option[Iterable[(P, Int)]]
}

/**Rich trait that defines full allele's data, where all non-mandatory fields must be filled. Trait can be directly
  * mixed with a[[it.unibo.pps.ese.controller.simulation.loader.data.SimulationData]]
  *
  * @tparam A Internal animals type(completion level)
  * @tparam P Internal plants type(completion level)
  */
trait FullSimulationData[A <: PartialAnimalData, P <: PartialPlantData] extends SimulationData[A, P] {
  /**
    * @throws IllegalStateException if property is not set
    * @return Simulation's animal with their quantity
    */
  @throws[IllegalStateException]
  def animals: Map[A, Int] = getAnimals.getOrElse(throw new IllegalStateException()).toMap
  /**
    * @throws IllegalStateException if property is not set
    * @return Simulation's plants with their quantity
    */
  @throws[IllegalStateException]
  def plants: Map[P, Int] = getPlants.getOrElse(throw new IllegalStateException()).toMap
}

object SimulationData {
  /** Type that defines partial simulation data's features*/
  type PartialSimulationData = SimulationData [_ <: PartialAnimalData, _ <: PartialPlantData]

  /** Trait that defines partial simulation data's features*/
  trait CompleteSimulationData extends FullSimulationData [CompleteAnimalData, CompletePlantData]
}
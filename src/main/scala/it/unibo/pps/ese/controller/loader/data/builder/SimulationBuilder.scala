package it.unibo.pps.ese.controller.loader.data.builder
//
//import it.unibo.pps.ese.controller.loader.data.{AnimalData, PlantData, SimulationData}
//import it.unibo.pps.ese.controller.loader.data.builder.SimulationBuilder.SimulationStatus
//import it.unibo.pps.ese.controller.loader.data.builder.SimulationBuilder.SimulationStatus.{FullSimulation, SimulationWithAnimals}
//
//
//class SimulationBuilder[T <: SimulationStatus](plants: Iterable[PlantData] = Seq(), animals: Iterable[AnimalData] = Seq()) {
//  def addAnimals(animals: Iterable[AnimalData]): SimulationBuilder[T with SimulationWithAnimals] =
//    new SimulationBuilder(plants, animals)
//
//  def addPlants(plants: Iterable[PlantData]): SimulationBuilder[T with SimulationWithAnimals] =
//    new SimulationBuilder(plants, animals)
//
//  def build(implicit ev: T =:= FullSimulation): SimulationData = ???
//}
//
//object SimulationBuilder {
//  sealed trait SimulationStatus
//  object SimulationStatus {
//    sealed trait EmptySimulation
//    sealed trait SimulationWithPlants
//    sealed trait SimulationWithAnimals
//
//    type FullSimulation = EmptySimulation with SimulationWithPlants with SimulationWithAnimals
//  }
//}

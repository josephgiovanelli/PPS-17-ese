package it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.simulation.loader.data.builder.SimulationBuilder

trait SimulationBuildFixture extends AnimalBuilderFixture with PlantBuildFixture {
  def simulationBFixture = new {
    val complete = SimulationBuilder()
      .addAnimals(Seq((animalBFixture.complete, 0)))
      .addPlants(Seq((plantBFixture.complete, 0)))

    val staticIncomplete = SimulationBuilder()
      .addPlants(Seq((plantBFixture.complete, 0)))

    val dynamicIncomplete = SimulationBuilder()
      .addAnimals(Seq((animalBFixture.dynamicIncomplete, 0)))
      .addPlants(Seq((plantBFixture.complete, 0)))

    val dynamicIncomplete1 = SimulationBuilder()
      .addAnimals(Seq((animalBFixture.complete, -1)))
      .addPlants(Seq((plantBFixture.complete, 0)))
  }
}

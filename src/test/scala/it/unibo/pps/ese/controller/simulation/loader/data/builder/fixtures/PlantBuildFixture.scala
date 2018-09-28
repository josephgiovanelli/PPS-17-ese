package it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.PlantBuilder

trait PlantBuildFixture {
  def plantBFixture = new {
    val complete = PlantBuilder()
      .setName("Plant")
      .setAlleleLength(3)
      .setGeneLength(3)
      .setHardness(5)
      .setHeight(5)
      .setNutritionalValue(5)
      .setReign("P")

    val staticIncomplete = PlantBuilder()
      .setName("Plant")
      .setAlleleLength(3)
      .setHardness(5)
      .setHeight(5)
      .setNutritionalValue(5)
      .setReign("P")

    val dynamicIncomplete = PlantBuilder()
      .setName("Plant")
      .setAlleleLength(3)
      .setGeneLength(-1)
      .setHardness(5)
      .setHeight(5)
      .setNutritionalValue(5)
      .setReign("P")
  }
}

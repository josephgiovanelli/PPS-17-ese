package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.builder.fixtures.AnimalBuilderFixture
import org.scalatest.WordSpec

class TestAnimalBuilder extends WordSpec with AnimalBuilderFixture {
  AnimalBuilder()
    .setName("Gatto")
    .setReign("M")
    .setTypology("")
    .setAlleleLength(5)
    .setGeneLength(5)
    .addStructuralChromosome(animalBFixture.completeCusChr)
    .addRegulationChromosome(animalBFixture.completeDefChr)
    .addSexualChromosome(animalBFixture.completeDefChr)
    .buildComplete
}

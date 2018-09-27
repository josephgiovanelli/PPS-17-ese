package it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.AnimalBuilder

trait AnimalBuilderFixture extends CustomGeneBuildFixture with DefaultGeneBuildFixture{
  def animalBFixture = new {
    val completeCusChr = Seq(customGBFixture.complete)
    val incompleteCusChr = Seq(customGBFixture.complete, customGBFixture.staticIncomplete)
    val completeDefChr = Seq(defaultGBFixture.complete)
    val incompleteDefChr = Seq(defaultGBFixture.complete, defaultGBFixture.dynamicIncomplete)
    val complete = AnimalBuilder()
      .setName("Gatto")
      .setReign("A")
      .setTypology("C")
      .setAlleleLength(5)
      .setGeneLength(5)
      .addStructuralChromosome(completeCusChr)
      .addRegulationChromosome(defaultGBFixture.completeRegulationChromosome)
      .addSexualChromosome(defaultGBFixture.completeSexualChromosome)
    val staticIncomplete = AnimalBuilder()
      .setName("Gatto")
      .setTypology("C")
      .setGeneLength(5)
      .addStructuralChromosome(completeCusChr)
      .addRegulationChromosome(defaultGBFixture.completeRegulationChromosome)
      .addSexualChromosome(defaultGBFixture.completeSexualChromosome)
    val dynamicIncomplete = AnimalBuilder()
      .setName("Gatto")
      .setReign("A")
      .setTypology("C")
      .setAlleleLength(5)
      .setGeneLength(5)
      .addStructuralChromosome(incompleteCusChr)
      .addRegulationChromosome(defaultGBFixture.completeRegulationChromosome)
      .addSexualChromosome(defaultGBFixture.completeSexualChromosome)
  }
}

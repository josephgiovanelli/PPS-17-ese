package it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder

trait AlleleBuildFixture {
  def alleleBFixture = new {
    val complete = AlleleBuilder()
      .setId("a")
      .setGene("")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)

    val incomplete = AlleleBuilder()
      .setId("a")
      .setGene("")
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)

    val wrong = AlleleBuilder()
      .setConsume(0)
      .setGene("")
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)

    val templateFullProb = AlleleBuilder()
      .setConsume(0)
      .setDominance(0)
      .setProbability(1.0)

    val templateHalfProb = AlleleBuilder()
      .setConsume(0)
      .setDominance(0)
      .setProbability(0.5)
  }
}

package it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.CustomGeneBuilder

trait CustomGeneBuildFixture extends AlleleBuildFixture {
  def customGBFixture = new {

    val complete = CustomGeneBuilder()
      .setId("idcc")
      .setName("namecc")
      .addAlleles(Set(
        alleleBFixture.templateFullProb
        .setId("a")
        .setGene("idcc")
        .setEffect(Map("test" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map("test" -> Map("height" -> 1)))

    val staticIncomplete = CustomGeneBuilder()
      .setName("namecsi")
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("a")
          .setGene("idcsi")
          .setEffect(Map("test" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map("test" -> Map("height" -> 1)))

    val dynamicIncomplete = CustomGeneBuilder()
      .setId("iddi")
      .setName("namecdi")
      .addAlleles(Set(
        alleleBFixture.templateHalfProb
          .setId("a")
          .setGene("iddi")
          .setEffect(Map("test" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map("test" -> Map("height" -> 1)))
  }
}

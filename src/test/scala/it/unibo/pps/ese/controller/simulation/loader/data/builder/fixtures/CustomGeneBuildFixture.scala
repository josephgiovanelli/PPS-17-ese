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
        .setId("")
        .setGene("idcc")
        .setEffect(Map("test" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map())

    val staticIncomplete = CustomGeneBuilder()
      .setName("namecsi")
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("")
          .setGene("idcsi")
          .setEffect(Map("test" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map())

    val dynamicIncomplete = CustomGeneBuilder()
      .setId("iddi")
      .setName("namecdi")
      .addAlleles(Set(
        alleleBFixture.templateHalfProb
          .setId("")
          .setGene("iddi")
          .setEffect(Map("test" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map())
  }
}

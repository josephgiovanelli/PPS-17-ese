package it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.DefaultGeneBuilder

trait DefaultGeneBuildFixture extends AlleleBuildFixture {
  def defaultGBFixture = new {

    val complete = DefaultGeneBuilder()
      .setId("iddc")
      .setName("namedc")
      .addProperties(Map("test" -> Double.getClass))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
        .setId("a")
        .setGene("iddc")
          .setEffect(Map("test" -> 1))
      ))

    val staticIncomplete = DefaultGeneBuilder()
      .setName("namedsi")
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("a")
          .setGene("iddc")
          .setEffect(Map("iddsi" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))

    val dynamicIncomplete = DefaultGeneBuilder()
      .setId("idddi")
      .setName("nameddi")
      .addAlleles(Set(
        alleleBFixture.templateHalfProb
          .setId("a")
          .setGene("idddi")
          .setEffect(Map("test" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))
  }
}

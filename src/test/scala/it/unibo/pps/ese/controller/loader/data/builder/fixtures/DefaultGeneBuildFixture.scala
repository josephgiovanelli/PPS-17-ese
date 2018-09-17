package it.unibo.pps.ese.controller.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.loader.data.builder.AlleleBuilder
import it.unibo.pps.ese.controller.loader.data.builder.gene.DefaultGeneBuilder

trait DefaultGeneBuildFixture extends AlleleBuildFixture {
  def defaultGBFixture = new {
    private val all1 = AlleleBuilder()
      .setId("")
      .setGene("iddc")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)

    private val all2 = AlleleBuilder()
      .setId("")
      .setGene("iddsi")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)

    private val all3 = AlleleBuilder()
      .setId("")
      .setGene("idddi")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(0.5)

    val complete = DefaultGeneBuilder()
      .setId("iddc")
      .setName("namedc")
      .addProperties(Map("test" -> Double.getClass))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
        .setId("")
        .setGene("iddc")
          .setEffect(Map("test" -> 1))
      ))

    val staticIncomplete = DefaultGeneBuilder()
      .setName("namedsi")
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("")
          .setGene("iddc")
          .setEffect(Map("iddsi" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))

    val dynamicIncomplete = DefaultGeneBuilder()
      .setId("idddi")
      .setName("nameddi")
      .addAlleles(Set(
        alleleBFixture.templateHalfProb
          .setId("")
          .setGene("idddi")
          .setEffect(Map("test" -> 1))
      ))
      .addProperties(Map("test" -> Double.getClass))
  }
}

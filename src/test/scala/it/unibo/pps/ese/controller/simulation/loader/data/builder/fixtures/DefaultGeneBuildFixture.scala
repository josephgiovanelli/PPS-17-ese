package it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.simulation.loader.data.builder.AlleleBuilder
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.DefaultGeneBuilder

trait DefaultGeneBuildFixture extends AlleleBuildFixture {
  def defaultGBFixture = new {

    val complete = DefaultGeneBuilder()
      .setId("iddc")
      .setName("namedc")
      .setProperties(Map("test" -> classOf[Double]))
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
      .setProperties(Map("test" -> classOf[Double]))

    val dynamicIncomplete = DefaultGeneBuilder()
      .setId("idddi")
      .setName("nameddi")
      .addAlleles(Set(
        alleleBFixture.templateHalfProb
          .setId("a")
          .setGene("idddi")
          .setEffect(Map("test" -> 1))
      ))
      .setProperties(Map("test" -> classOf[Double]))

    private val maturity = DefaultGeneBuilder()
      .setId("aaa")
      .setName("maturity")
      .setProperties(Map("maturity" -> classOf[Double]))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("aaa")
          .setGene("aaa")
          .setEffect(Map("maturity" -> 1))
      ))

    private val life = DefaultGeneBuilder()
      .setId("aab")
      .setName("life")
      .setProperties(Map("life" -> classOf[Double]))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("aab")
          .setGene("aab")
          .setEffect(Map("life" -> 1))
      ))

    private val childhood = DefaultGeneBuilder()
      .setId("aac")
      .setName("childhood")
      .setProperties(Map("childhood" -> classOf[Double]))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("aac")
          .setGene("aac")
          .setEffect(Map("childhood" -> 1))
      ))

    private val oldness = DefaultGeneBuilder()
      .setId("aad")
      .setName("oldness")
      .setProperties(Map("oldness" -> classOf[Double]))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("aad")
          .setGene("aad")
          .setEffect(Map("oldness" -> 1))
      ))

    private val decline = DefaultGeneBuilder()
      .setId("aae")
      .setName("decline")
      .setProperties(Map("decline" -> classOf[Double]))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("aae")
          .setGene("aae")
          .setEffect(Map("decline" -> 1))
      ))

    val completeRegulationChromosome = Seq(maturity, life, childhood, oldness, decline)

    val fertility = DefaultGeneBuilder()
      .setId("aaf")
      .setName("fertility")
      .setProperties(Map("fertility" -> classOf[Double]))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("aaf")
          .setGene("aaf")
          .setEffect(Map("fertility" -> 1))
      ))

    val fecundity = DefaultGeneBuilder()
      .setId("aag")
      .setName("fecundity")
      .setProperties(Map("fecundity" -> classOf[Double]))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("aag")
          .setGene("aag")
          .setEffect(Map("fecundity" -> 1))
      ))

    val pregnancyDuration = DefaultGeneBuilder()
      .setId("aah")
      .setName("pregnancyDuration")
      .setProperties(Map("pregnancyDuration" -> classOf[Double]))
      .addAlleles(Set(
        alleleBFixture.templateFullProb
          .setId("aah")
          .setGene("aah")
          .setEffect(Map("pregnancyDuration" -> 1))
      ))

    val completeSexualChromosome = Seq(fertility, fecundity, pregnancyDuration)
  }
}

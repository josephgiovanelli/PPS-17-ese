package it.unibo.pps.ese.controller.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.CompleteAlleleData
import it.unibo.pps.ese.controller.loader.data.builder.{AlleleBuilder, GeneBuilder}

trait DefaultGeneBuildFixture {
  def defaultGBFixture = new {
    private val all1 = AlleleBuilder()
      .setId("")
      .setGene("iddc")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)
      .buildComplete

    private val all2 = AlleleBuilder()
      .setId("")
      .setGene("iddsi")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)
      .buildComplete

    private val all3 = AlleleBuilder()
      .setId("")
      .setGene("idddi")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(0.5)
      .buildComplete

    val complete = GeneBuilder()
      .setId("iddc")
      .setName("namedc")
      .addProperties(Map("test" -> Double.getClass))
      .addAlleles(Set[CompleteAlleleData](all1))

    val staticIncomplete = GeneBuilder()
      .setId("iddsi")
      .addAlleles(Set[CompleteAlleleData](all2))
      .addProperties(Map("test" -> Double.getClass))

    val dynamicIncomplete = GeneBuilder()
      .setId("idddi")
      .setName("nameddi")
      .addAlleles(Set[CompleteAlleleData](all3))
      .addProperties(Map("test" -> Double.getClass))
  }
}

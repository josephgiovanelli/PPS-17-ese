package it.unibo.pps.ese.controller.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.AlleleData
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder

trait CustomGeneBuildFixture {
  def customGBFixture = new {
    val complete = GeneBuilder()
      .setId("idcc")
      .setName("namecc")
      .addAlleles(Set[AlleleData](Allele("idcc", "", 0, 0, 1.0, Map())))
      .addProperties(Map())
      .addConversionMap(Map())

    val staticIncomplete = GeneBuilder()
      .setId("idcsi")
      .addAlleles(Set[AlleleData](Allele("idcsi", "", 0, 0, 1.0, Map())))
      .addProperties(Map())
      .addConversionMap(Map())

    val dynamicIncomplete = GeneBuilder()
      .setId("iddi")
      .setName("namecdi")
      .addAlleles(Set[AlleleData](Allele("iddi", "", 0, 0, 0.5, Map())))
      .addProperties(Map())
      .addConversionMap(Map())
  }
}

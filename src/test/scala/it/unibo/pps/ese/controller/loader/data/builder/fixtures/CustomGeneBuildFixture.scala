package it.unibo.pps.ese.controller.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.AlleleData
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder

trait CustomGeneBuildFixture {
  def customGBFixture = new {
    val complete = GeneBuilder()
      .setId("")
      .setName("")
      .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 1.0, Map())))
      .addProperties(Map())
      .addConversionMap(Map())

    val staticIncomplete = GeneBuilder()
      .setId("")
      .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 1.0, Map())))
      .addProperties(Map())
      .addConversionMap(Map())

    val dynamicIncomplete = GeneBuilder()
      .setId("")
      .setName("")
      .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 0.5, Map())))
      .addProperties(Map())
      .addConversionMap(Map())
  }
}

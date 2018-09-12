package it.unibo.pps.ese.controller.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.AlleleData
import it.unibo.pps.ese.controller.loader.data.builder.GeneBuilder

trait DefaultGeneBuildFixture {
  def defaultGBFixture = new {
    val complete = GeneBuilder()
      .setId("iddc")
      .setName("namedc")
      .addProperties(Map())
      .addAlleles(Set[AlleleData](Allele("iddc", "", 0, 0, 1.0, Map())))

    val staticIncomplete = GeneBuilder()
      .setId("iddsi")
      .addAlleles(Set[AlleleData](Allele("iddsi", "", 0, 0, 1.0, Map())))
      .addProperties(Map())

    val dynamicIncomplete = GeneBuilder()
      .setId("idddi")
      .setName("nameddi")
      .addAlleles(Set[AlleleData](Allele("idddi", "", 0, 0, 0.5, Map())))
      .addProperties(Map())
  }
}

package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData

object DataValidator {
  def validateNewSimulationData(data: CompleteSimulationData): Unit = {
    /*
     * simulation content test
     */
    assert(data.animals.size == 3)
    assert(data.plants.size == 2)
    val tyrOpt = data.animals.keys.find(_.name == "Tyrannosaurus")
    assert(tyrOpt.nonEmpty)
    val felceOpt = data.plants.keys.find(_.name == "Felce")
    assert(felceOpt.nonEmpty)
    /*
     * Animal content test
     */
    val tyr = tyrOpt.get
    assert(tyr.structuralChromosome.size == 2)
    assert(tyr.regulationChromosome.size == 5)
    assert(tyr.sexualChromosome.size == 3)
    assert(tyr.alleleLength == 3)
    assert(tyr.geneLength == 3)
    assert(tyr.reign == "A")
    assert(tyr.typology == "C")
    val gamOpt = tyr.structuralChromosome.find(_.name == "gambe")
    assert(gamOpt.nonEmpty)
    assert(gamOpt.get.alleles.size == 3)
    val occOtp = tyr.structuralChromosome.find(_.name == "occhi")
    assert(occOtp.nonEmpty)
    /*
     * gene content test
     */
    val occ = occOtp.get
    assert(occ.alleles.size == 2)
    assert(occ.id == "occ")
    assert(occ.properties.keySet == Set("view", "beauty"))
    assert(occ.conversionMap.keySet == occ.properties.keySet)
    assert(occ.conversionMap("view").size == 1)
    assert(occ.conversionMap("view").contains("fieldOfView"))
    assert(occ.conversionMap("view")("fieldOfView") == 7.5)
    val occAll = occ.alleles
    assert(occ.alleles.size == 2)
    val allOpt = occAll.find(_.id == "sda")
    assert(allOpt.nonEmpty)
    /*
     * allele content test
     */
    val all = allOpt.get
    assert(all.gene == "occ")
    assert(all.dominance == 3)
    assert(all.consume == 2)
    assert(all.probability == 1)
    assert(all.effect.keySet == Set("view", "beauty"))
    assert(all.effect("view") == 3)
    assert(all.effect("beauty") == 3)
    /*
     * Plant content test
     */
    val felce = felceOpt.get
    assert(felce.geneLength == 3)
    assert(felce.alleleLength == 3)
    assert(felce.reign == "P")
    assert(felce.height == 10)
    assert(felce.nutritionalValue == 8)
    assert(felce.hardness == 2)
  }

}

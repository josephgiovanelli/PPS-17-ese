package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.{AlleleData, CompleteDefaultGeneData, PartialDefaultGeneData}
import org.scalatest.FunSuite

class TestGeneBuilder extends FunSuite {

  private val builder = GeneBuilder()
    .setId("")
    .setName("")
    .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 1.0, Map())))
    .addProperties(Map())

  private val incompleteStaticDefaultBuilder = GeneBuilder()
    .setId("")
    .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 1.0, Map())))
    .addProperties(Map())

  private val incompleteDynamicDefaultBuilder = GeneBuilder()
    .setId("")
    .setName("")
    .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 0.5, Map())))
    .addProperties(Map())

  test("An explicit complete build of default gene is possible") {
    builder.buildCompleteDefault match {
      case gb: CompleteDefaultGeneData =>
      case _ =>
        fail()
    }
  }

  test("An implicit complete build of default gene is possible") {
    builder.buildDefault match {
      case gb: CompleteDefaultGeneData =>
      case _ =>
        fail()
    }
  }

  test("Builder with missing params creates incomplete gene with implicit build") {
    incompleteStaticDefaultBuilder.buildDefault match {
      case gb: PartialDefaultGeneData =>
      case _ =>
        fail()
    }
  }

  test("Builder with incomplete params creates incomplete gene with implicit build") {
    incompleteDynamicDefaultBuilder.buildDefault match {
      case gb: PartialDefaultGeneData =>
      case _ =>
        fail()
    }
  }

  test("Builder with incomplete params throws exception in explicit complete build") {
    assertThrows[IllegalStateException](incompleteDynamicDefaultBuilder.buildCompleteDefault)
  }

}

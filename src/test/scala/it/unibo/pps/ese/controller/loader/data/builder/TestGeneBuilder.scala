package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data._
import org.scalatest.WordSpec

class TestGeneBuilder extends WordSpec {

  private val completeDefaultBuilder = GeneBuilder()
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

  private val completeCustomBuilder = GeneBuilder()
    .setId("")
    .setName("")
    .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 1.0, Map())))
    .addProperties(Map())
    .addConversionMap(Map())

  private val incompleteStaticCustomBuilder = GeneBuilder()
    .setId("")
    .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 1.0, Map())))
    .addProperties(Map())
    .addConversionMap(Map())

  private val incompleteDynamicCustomBuilder = GeneBuilder()
    .setId("")
    .setName("")
    .addAlleles(Set[AlleleData](Allele("", "", 0, 0, 0.5, Map())))
    .addProperties(Map())
    .addConversionMap(Map())

  "A GeneBuilder" when {
    "used to build a Default Gene" when {
      "is correctly filled" should {
        "explicitly build correctly" in {
          completeDefaultBuilder.buildCompleteDefault match {
            case gb: CompleteDefaultGeneData =>
            case _ =>
              fail()
          }
        }
        "implicitly build correctly" in {
          completeDefaultBuilder.buildDefault match {
            case gb: CompleteDefaultGeneData =>
            case _ =>
              fail()
          }
        }
      }

      "has missing parameters" should {
        "implicitly build a PartialGeneData" in {
          incompleteStaticDefaultBuilder.buildDefault match {
            case gb: PartialDefaultGeneData =>
            case _ =>
              fail()
          }
        }
      }

      "isn't filled correctly" should {
        "implicitly build a PartialGeneData" in {
          incompleteDynamicDefaultBuilder.buildDefault match {
            case gb: PartialDefaultGeneData =>
            case _ =>
              fail()
          }
        }
        "throw an exception if explicitly build as complete" in {
          assertThrows[IllegalStateException](incompleteDynamicDefaultBuilder.buildCompleteDefault)
        }
      }
    }
    "used to build a Custom Gene" when {
      "is correctly filled" should {
        "explicitly build correctly" in {
          completeCustomBuilder.buildCompleteCustom match {
            case gb: CompleteCustomGeneData =>
            case _ =>
              fail()
          }
        }
        "implicitly build correctly" in {
          completeCustomBuilder.buildCustom match {
            case gb: CompleteCustomGeneData =>
            case _ =>
              fail()
          }
        }
      }

      "has missing parameters" should {
        "implicitly build a PartialGeneData" in {
          incompleteStaticCustomBuilder.buildCustom match {
            case gb: PartialCustomGeneData =>
            case _ =>
              fail()
          }
        }
      }

      "isn't filled correctly" should {
        "implicitly build a PartialGeneData" in {
          incompleteDynamicCustomBuilder.buildCustom match {
            case gb: PartialCustomGeneData =>
            case _ =>
              fail()
          }
        }
        "throw an exception if explicitly build as complete" in {
          assertThrows[IllegalStateException](incompleteDynamicDefaultBuilder.buildCompleteDefault)
        }
      }
    }
  }
}

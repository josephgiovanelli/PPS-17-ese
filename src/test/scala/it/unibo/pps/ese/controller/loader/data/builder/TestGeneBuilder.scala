package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.loader.data.builder.fixtures.{CustomGeneBuildFixture, DefaultGeneBuildFixture}
import org.scalatest.WordSpec

class TestGeneBuilder extends WordSpec with DefaultGeneBuildFixture with CustomGeneBuildFixture {

  "A GeneBuilder" when {
    "used to build a Default Gene" when {
      "is correctly filled" should {
        "explicitly build correctly" in {
          defaultGBFixture.complete.buildCompleteDefault match {
            case gb: CompleteDefaultGeneData =>
            case _ =>
              fail()
          }
        }
        "implicitly build correctly" in {
          defaultGBFixture.complete.buildDefault match {
            case gb: CompleteDefaultGeneData =>
            case _ =>
              fail()
          }
        }
        //TODO generate gene with correct data
        //TODO expl and impl build correctly if filled with default gene data
      }

      "has missing parameters" should {
        "implicitly build a PartialGeneData" in {
          defaultGBFixture.staticIncomplete.buildDefault match {
            case gb: CompleteDefaultGeneData =>
              fail()
            case gb: PartialDefaultGeneData =>
            case _ =>
              fail()
          }
        }
      }

      "isn't filled correctly" should {
        "implicitly build a PartialGeneData" in {
          defaultGBFixture.dynamicIncomplete.buildDefault match {
            case gb: CompleteDefaultGeneData =>
              fail()
            case gb: PartialDefaultGeneData =>
            case _ =>
              fail()
          }
        }
        "throw an exception if explicitly build as complete" in {
          assertThrows[CompleteBuildException](defaultGBFixture.dynamicIncomplete.buildCompleteDefault)
        }
      }
    }
    "used to build a Custom Gene" when {
      "is correctly filled" should {
        "explicitly build correctly" in {
          customGBFixture.complete.buildCompleteCustom match {
            case gb: CompleteCustomGeneData =>
            case _ =>
              fail()
          }
        }
        "implicitly build correctly" in {
          customGBFixture.complete.buildCustom match {
            case gb: CompleteCustomGeneData =>
            case _ =>
              fail()
          }
        }
      }

      "has missing parameters" should {
        "implicitly build a PartialGeneData" in {
          customGBFixture.staticIncomplete.buildCustom match {
            case gb: CompleteCustomGeneData =>
              fail()
            case gb: PartialCustomGeneData =>
            case _ =>
              fail()
          }
        }
      }

      "isn't filled correctly" should {
        "implicitly build a PartialGeneData" in {
          customGBFixture.dynamicIncomplete.buildCustom match {
            case gb: CompleteCustomGeneData =>
              fail()
            case gb: PartialCustomGeneData =>
            case _ =>
              fail()
          }
        }
        "throw an exception if explicitly build as complete" in {
          assertThrows[CompleteBuildException](customGBFixture.dynamicIncomplete.buildCompleteCustom)
        }
      }
    }
  }
}

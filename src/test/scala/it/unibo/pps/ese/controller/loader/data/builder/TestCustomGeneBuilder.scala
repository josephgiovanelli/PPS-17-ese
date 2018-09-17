package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.loader.data.builder.fixtures.CustomGeneBuildFixture
import org.scalatest.WordSpec

class TestCustomGeneBuilder extends WordSpec with CustomGeneBuildFixture {
  "A CustomGeneBuilder" when {
    "is correctly filled" should {
      "explicitly build correctly" in {
        customGBFixture.complete.buildComplete match {
          case gb: CompleteCustomGeneData =>
          case _ =>
            fail()
        }
      }
      "implicitly build correctly" in {
        customGBFixture.complete.build match {
          case gb: CompleteCustomGeneData =>
          case _ =>
            fail()
        }
      }
    }

    "has missing parameters" should {
      "implicitly build a PartialGeneData" in {
        customGBFixture.staticIncomplete.build match {
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
        customGBFixture.dynamicIncomplete.build match {
          case gb: CompleteCustomGeneData =>
            fail()
          case gb: PartialCustomGeneData =>
          case _ =>
            fail()
        }
      }
      "throw an exception if explicitly build as complete" in {
        assertThrows[CompleteBuildException](customGBFixture.dynamicIncomplete.buildComplete)
      }
    }
  }
}

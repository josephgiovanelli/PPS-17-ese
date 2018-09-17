package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}
import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.loader.data.builder.fixtures.DefaultGeneBuildFixture
import org.scalatest.WordSpec

class TestDefaultGeneBuilder extends WordSpec with DefaultGeneBuildFixture {

  "A DefaultGeneBuilder" when {
    "is correctly filled" should {
      "explicitly build correctly" in {
        defaultGBFixture.complete.buildComplete match {
          case gb: CompleteDefaultGeneData =>
          case _ =>
            fail()
        }
      }
      "implicitly build correctly" in {
        defaultGBFixture.complete.build match {
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
        defaultGBFixture.staticIncomplete.build match {
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
        defaultGBFixture.dynamicIncomplete.build match {
          case gb: CompleteDefaultGeneData =>
            fail()
          case gb: PartialDefaultGeneData =>
          case _ =>
            fail()
        }
      }
      "throw an exception if explicitly build as complete" in {
        assertThrows[CompleteBuildException](defaultGBFixture.dynamicIncomplete.buildComplete)
      }
    }
  }

}

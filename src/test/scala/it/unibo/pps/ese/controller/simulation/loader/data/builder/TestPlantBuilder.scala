package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.{CompletePlantData, PartialPlantData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures.PlantBuildFixture
import org.scalatest.WordSpec

class TestPlantBuilder extends WordSpec with PlantBuildFixture {
  "A PlantBuilder" when {
    "is correctly filled" should {
      "explicitly build correctly" in {
        plantBFixture.complete.buildComplete match {
          case gb: CompletePlantData =>
          case _ =>
            fail()
        }
      }
      "implicitly build correctly" in {
        plantBFixture.complete.build() match {
          case gb: CompletePlantData =>
          case _ =>
            fail()
        }
      }
    }

    "has missing parameters" should {
      "implicitly build a PartialPlantData" in {
        plantBFixture.staticIncomplete.build() match {
          case gb: CompletePlantData =>
            fail()
          case gb: PartialPlantData =>
          case _ =>
            fail()
        }
      }
    }

    "isn't filled correctly" should {
      "implicitly build a PartialPlantData" in {
        plantBFixture.dynamicIncomplete.build() match {
          case gb: CompletePlantData =>
            fail()
          case gb: PartialPlantData =>
          case _ =>
            fail()
        }
      }
      "throw an exception if explicitly build as complete" in {
        assertThrows[CompleteBuildException](plantBFixture.dynamicIncomplete.buildComplete)
      }
    }
  }
}

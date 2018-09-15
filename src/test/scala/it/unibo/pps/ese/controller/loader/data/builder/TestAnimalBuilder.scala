package it.unibo.pps.ese.controller.loader.data.builder

import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.loader.data.builder.fixtures.AnimalBuilderFixture
import org.scalatest.WordSpec

class TestAnimalBuilder extends WordSpec with AnimalBuilderFixture {
  "A GeneBuilder" when {
    "is correctly filled" should {
      "explicitly build correctly" in {
        animalBFixture.complete.buildComplete match {
          case gb: CompleteAnimalData =>
          case _ =>
            fail()
        }
      }
      "implicitly build correctly" in {
        animalBFixture.complete.build() match {
          case gb: CompleteAnimalData =>
          case _ =>
            fail()
        }
      }
      //TODO generate gene with correct data
      //TODO expl and impl build correctly if filled with default gene data
    }

    "has missing parameters" should {
      "implicitly build a PartialGeneData" in {
        animalBFixture.staticIncomplete.build() match {
          case gb: CompleteAnimalData =>
            fail()
          case gb: PartialAnimalData =>
          case _ =>
            fail()
        }
      }
    }

    "isn't filled correctly" should {
      "implicitly build a PartialGeneData" in {
        animalBFixture.dynamicIncomplete.build() match {
          case gb: CompleteAnimalData =>
            fail()
          case gb: PartialAnimalData =>
          case _ =>
            fail()
        }
      }
      "throw an exception if explicitly build as complete" in {
        assertThrows[CompleteBuildException](animalBFixture.dynamicIncomplete.buildComplete)
      }
    }
  }
}

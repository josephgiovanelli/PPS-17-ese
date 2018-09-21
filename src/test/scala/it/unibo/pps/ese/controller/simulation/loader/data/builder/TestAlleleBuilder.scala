package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.{CompleteAlleleData, PartialAlleleData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures.AlleleBuildFixture
import org.scalatest.WordSpec

class TestAlleleBuilder extends WordSpec with AlleleBuildFixture {
  "An AlleleBuilder" when {
    "is correctly filled" should {
      "explicitly build correctly" in {
        alleleBFixture.complete.buildComplete match {
          case gb: CompleteAlleleData =>
          case _ =>
            fail()
        }
      }
      "implicitly build correctly" in {
        alleleBFixture.complete.build match {
          case gb: CompleteAlleleData =>
          case _ =>
            fail()
        }
      }
    }

    "has missing parameters" should {
      "implicitly build a PartialAlleleData" in {
        alleleBFixture.incomplete.build match {
          case gb: CompleteAlleleData =>
            fail()
          case gb: PartialAlleleData =>
          case _ =>
            fail()
        }
      }
    }

    "has not all mandatory fields set" should {
      "throw an exception if build" in {
        assertThrows[Exception](alleleBFixture.wrong.build)
      }
    }
  }
}

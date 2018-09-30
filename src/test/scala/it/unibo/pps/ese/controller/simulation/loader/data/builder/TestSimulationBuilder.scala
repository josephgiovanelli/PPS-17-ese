package it.unibo.pps.ese.controller.simulation.loader.data.builder

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException
import it.unibo.pps.ese.controller.simulation.loader.data.builder.fixtures.SimulationBuildFixture
import org.scalatest.WordSpec

class TestSimulationBuilder extends WordSpec with SimulationBuildFixture {
  "A SimulationBuilder" when {
    "is correctly filled" should {
      "explicitly build correctly" in {
        simulationBFixture.complete.buildComplete match {
          case gb: CompleteSimulationData =>
          case _ =>
            fail()
        }
      }
      "implicitly build correctly" in {
        simulationBFixture.complete.build() match {
          case gb: CompleteSimulationData =>
          case _ =>
            fail()
        }
      }
    }

    "has missing parameters" should {
      "implicitly build a PartialSimulationData" in {
        simulationBFixture.staticIncomplete.build() match {
          case gb: CompleteSimulationData =>
            fail()
          case gb: PartialSimulationData =>
          case _ =>
            fail()
        }
      }
    }

    "isn't filled correctly" should {
      "implicitly build a PartialSimulationData" in {
        simulationBFixture.dynamicIncomplete.build() match {
          case gb: CompleteSimulationData =>
            fail()
          case gb: PartialSimulationData =>
          case _ =>
            fail()
        }

        simulationBFixture.dynamicIncomplete1.build() match {
          case gb: CompleteSimulationData =>
            fail()
          case gb: PartialSimulationData =>
          case _ =>
            fail()
        }
      }
      "throw an exception if explicitly build as complete" in {
        assertThrows[CompleteBuildException](simulationBFixture.dynamicIncomplete.buildComplete)
        assertThrows[CompleteBuildException](simulationBFixture.dynamicIncomplete1.buildComplete)
      }
    }
  }
}

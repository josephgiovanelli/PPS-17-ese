package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.simulation.loader.io.File
import org.kaikikm.threadresloader.ResourceLoader
import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class TestYamlLoader extends FunSuite {
  test("YamlLoader.loadSimulation") {
    YamlLoader.loadCompleteSimulation(File(ResourceLoader
      .getResource("it/unibo/pps/ese/controller/simulation/loader/newsimulation/Simulation.yml"))) match {
      case Success(value) =>
        DataValidator.validateNewSimulationData(value)
      case Failure(_) =>
        fail()
    }
  }
}

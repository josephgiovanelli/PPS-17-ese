package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.simulation.loader.io.File
import org.kaikikm.threadresloader.ResourceLoader
import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class TestYamlLoader extends FunSuite {
  test("YamlLoader.loadSimulation") {
    val data: CompleteSimulationData = YamlLoader.loadCompleteSimulation(File(ResourceLoader
      .getResource("it/unibo/pps/ese/controller/simulation/loader/newsimulation/Simulation.yml"))) match {
      case Success(value) =>
        value
      case Failure(_) =>
        fail()
    }

    assert(data.animals.size == 3)
    assert(data.plants.size == 2)
    val tyrOpt = data.animals.keys.find(_.name == "Tyrannosaurus")
    assert(tyrOpt.nonEmpty)
    val tyr = tyrOpt.get
    assert(tyr.structuralChromosome.size == 2)
    assert(tyr.regulationChromosome.size == 5)
    assert(tyr.sexualChromosome.size == 3)
    val occOtp = tyr.structuralChromosome.find(_.name == "occhi")
    assert(occOtp.nonEmpty)
    assert(occOtp.get.alleles.size == 2)
    val gamOpt = tyr.structuralChromosome.find(_.name == "gambe")
    assert(gamOpt.nonEmpty)
    assert(gamOpt.get.alleles.size == 3)
  }
}

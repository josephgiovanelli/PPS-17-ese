package it.unibo.pps.ese.controller.simulation.loader

import it.unibo.pps.ese.controller.simulation.loader.io.File
import org.kaikikm.threadresloader.ResourceLoader
import org.scalatest.FunSuite

class TestYamlLoader extends FunSuite {
  test("YamlLoader.loadSimulation") {
    YamlLoader.loadSimulation(File(ResourceLoader.getResource("it/unibo/pps/ese/controller/loader/Simulation.yml")))
  }
}

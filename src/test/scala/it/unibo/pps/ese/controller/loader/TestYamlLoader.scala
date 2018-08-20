package it.unibo.pps.ese.controller.loader

import org.scalatest.FunSuite

class TestYamlLoader extends FunSuite {
  test("YamlLoader.loadSimulation") {
    new YamlLoader().loadSimulation("it/unibo/pps/ese/controller/loader/Simulation.yml")
  }
}

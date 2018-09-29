package it.unibo.pps.ese.controller.simulation.loader


import java.nio.file.Files

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.simulation.loader.io.{File, Folder}
import org.apache.commons.io.FileUtils
import org.kaikikm.threadresloader.ResourceLoader
import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class TestYamlSaver extends FunSuite {
  test("A saved simulation can be correctly re-loaded") {
    val data: CompleteSimulationData = YamlLoader.loadCompleteSimulation(File(ResourceLoader
      .getResource("it/unibo/pps/ese/controller/simulation/loader/newsimulation/Simulation.yml"))) match {
      case Success(value) =>
        value
      case Failure(_) =>
        fail()
    }

    DataValidator.validateNewSimulationData(data)
    val tmpFolder = Folder(Files.createTempDirectory("ese").toFile)
    val saver = YamlSaver(data, "test")
    saver.saveData(tmpFolder, false)
    tmpFolder.getChildren("test.yml") match {
      case f: File =>
        YamlLoader.loadCompleteSimulation(f) match {
          case Success(value) =>
            DataValidator.validateNewSimulationData(value)
          case _ =>
            fail()
        }
      case _ =>
        fail()
    }

    FileUtils.deleteDirectory(tmpFolder.rawFile)
  }
}

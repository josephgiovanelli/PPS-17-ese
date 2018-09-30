package it.unibo.pps.ese.controller.simulation.loader


import java.nio.file.Files

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.simulation.loader.exception.ResourceAlreadyExistsException
import it.unibo.pps.ese.controller.simulation.loader.io.{File, Folder, NotExistingFile, NotExistingFolder}
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
    saver.saveData(tmpFolder, overrideAll = false)
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

  test("An already existing resource causes expected failure. Resource can be added to override list") {
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
    tmpFolder.getChildren("test.yml") match {
      case f: NotExistingFile =>
        f.createFile()
      case _ =>
        fail()
    }
    saver.saveData(tmpFolder, overrideAll = false) match {
      case Success(_) =>
        fail()
      case Failure(exception: ResourceAlreadyExistsException) =>
        saver.addResourceToOverride(exception.existingResource)
        saver.saveData(tmpFolder, overrideAll = false) match {
          case Failure(_) =>
            fail()
          case _ =>
        }
      case _ =>
        fail()
    }
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

package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.{YamlLoader, YamlSaver}
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.util.io.Folder
import org.kaikikm.threadresloader.ResourceLoader

object TestSaver extends App {
  val loadedData: PartialSimulationData = YamlLoader.loadSimulation("it/unibo/pps/ese/controller/loader/Simulation.yml")
  val saver = YamlSaver(loadedData, "testttt")
  println(ResourceLoader.getResource("it/unibo/pps/ese/controller/testt/pincopallo.txt"))
  val targetFolder = Folder(ResourceLoader.getResource("it/unibo/pps/ese/controller/testt"))
  saver.saveData(targetFolder, false)
}

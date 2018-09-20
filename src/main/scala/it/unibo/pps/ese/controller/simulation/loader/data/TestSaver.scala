package it.unibo.pps.ese.controller.simulation.loader.data

import it.unibo.pps.ese.controller.simulation.loader.{YamlLoader, YamlSaver}
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.simulation.loader.io.{File, Folder}
import org.kaikikm.threadresloader.ResourceLoader

object TestSaver extends App {
  val loadedData: PartialSimulationData = YamlLoader.loadSimulation(File(ResourceLoader.getResource("it/unibo/pps/ese/controller/loader/Simulation.yml")))
  val saver = YamlSaver(loadedData, "testttt")
  println(ResourceLoader.getResource("it/unibo/pps/ese/controller/testt/pincopallo.txt"))
  val targetFolder = Folder("C:\\test\\")
  saver.saveData(targetFolder, false)
  YamlLoader.loadSimulation(File("C:\\test\\testttt.yml"))
}

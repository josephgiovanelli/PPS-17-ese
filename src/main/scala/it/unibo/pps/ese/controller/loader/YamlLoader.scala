package it.unibo.pps.ese.controller.loader

import it.unibo.pps.ese.controller.loader.beans.{Animal, DefaultChromosomeData, Plant, Simulation}
import net.jcazevedo.moultingyaml._
import org.kaikikm.threadresloader.ResourceLoader



class YamlLoader extends Loader {


  object CustomYaml extends DefaultYamlProtocol {
    implicit val simulationFormat = yamlFormat2(Simulation)
    implicit val plantFormat = yamlFormat6(Plant)
    implicit val defaultChromosomeDataFormat = yamlFormat2(DefaultChromosomeData)
    implicit val animalFormat = yamlFormat7(Animal)
  }

  import CustomYaml._

  override def loadSimulation(configPath: String): Unit = {
    val simulation = loadFileContent(configPath).parseYaml.convertTo[Simulation]
    val plants = simulation.plants.map({case (k, v) => (loadPlant(k), v)})
    val animals = simulation.animals.map({case (k, v) => (loadAnimal(k), v)})
    print(animals)
  }

  private def loadPlant(path: String): Plant = {
    loadFileContent(path).parseYaml.convertTo[Plant]
  }

  private def loadAnimal(path: String): Animal = {
    loadFileContent(path).parseYaml.convertTo[Animal]
  }

  private def loadFileContent(path: String): String = {
    scala.io.Source.fromInputStream(ResourceLoader.getResourceAsStream(path)).mkString
  }
}

case class SimulationData(animals: Map[Animal, Int], plants: Map[Plant, Int])

trait GeneData {
  def name: String
  def attributes: Seq[String]
}
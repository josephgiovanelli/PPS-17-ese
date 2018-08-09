package it.unibo.pps.ese.controller.loader

import java.io.InputStream

import it.unibo.pps.ese.controller.loader.beans._
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.util.io.Folder
import net.jcazevedo.moultingyaml._
import org.kaikikm.threadresloader.ResourceLoader


class YamlLoader extends Loader {

  object CustomYaml extends DefaultYamlProtocol {
    implicit val simulationFormat = yamlFormat2(Simulation)
    implicit val plantFormat = yamlFormat6(Plant)
    implicit val defaultChromosomeDataFormat = yamlFormat2(DefaultChromosomeData)
    implicit val animalFormat = yamlFormat7(Animal)
    implicit val propertyInfoFormat = yamlFormat1(PropertyInfo)
    implicit val geneFormat = yamlFormat4(Gene)
    implicit val alleleFormat = yamlFormat6(Allele)
  }

  import CustomYaml._

  override def loadSimulation(configPath: String): Unit = {
    val simulation = loadFileContent(configPath).parseYaml.convertTo[Simulation]
    val plants = simulation.plants.map({case (k, v) => (loadPlant(k), v)})
    val animals = simulation.animals.map({case (k, v) => (loadAnimal(k), v)})
  }

  private def loadPlant(path: String): Plant = loadFileContent(path).parseYaml.convertTo[Plant]

  private def loadAnimal(path: String): AnimalData = {
    val loadedAnimal = loadFileContent(path).parseYaml.convertTo[Animal]
    val structuralChromosome = loadStructuralChromosome(loadedAnimal.structuralChromosome)
    val regulationChromosome = loadDefaultChromosome(RegulationDefaultGenes.elements, loadedAnimal.regulationChromosome)
    val sexualChromosome = loadDefaultChromosome(SexualDefaultGenes.elements, loadedAnimal.sexualChromosome)
    AnimalData(loadedAnimal, structuralChromosome, regulationChromosome, sexualChromosome)
  }

  private def loadDefaultChromosome[T <: DefaultGene](genesSet: Set[T], chromosomeData: DefaultChromosomeData): Seq[DefaultGeneData] = {
    require(chromosomeData.names.keySet == genesSet.map(_.name))
    val alleles = loadAlleles(chromosomeData.allelesPath)
    //TODO check no wrong alleles
    chromosomeData.names.toSeq.map({
      case (k, v) => DefaultGeneData(genesSet.find(e => e.name == k).get, v, alleles.filter(a => a.gene == v))
    })
  }

  private def loadStructuralChromosome(genesPath: String): Seq[CustomGeneData] =  {
    Folder(genesPath).getFilesAsStream(Folder.YAML)
      .map(loadFileContent(_).parseYaml.convertTo[Gene])
      .map(g => CustomGeneData(g, loadAlleles(g.allelesPath)))
  }

  private def loadAlleles(allelesPath: String): Seq[AlleleData] = {
    Folder(allelesPath).getFilesAsStream(Folder.YAML)
      .map(loadFileContent(_).parseYaml.convertTo[Allele])
  }

  private def loadFileContent(path: String): String = {
    loadFileContent(ResourceLoader.getResourceAsStream(path))
  }

  private def loadFileContent(input: InputStream): String = {
    val source = scala.io.Source.fromInputStream(input)
    val ret = source.mkString
    source.close()
    ret
  }
}

case class SimulationData(animals: Map[Animal, Int], plants: Map[Plant, Int])
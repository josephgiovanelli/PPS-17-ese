package it.unibo.pps.ese.controller.loader

import java.io.InputStream

import it.unibo.pps.ese.controller.loader.beans._
import it.unibo.pps.ese.controller.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder.{AnimalBuilder, GeneBuilder, PlantBuilder, SimulationBuilder}
import it.unibo.pps.ese.controller.util.io.Folder
import net.jcazevedo.moultingyaml._
import org.kaikikm.threadresloader.ResourceLoader


object YamlLoader extends Loader {

  object CustomYaml extends DefaultYamlProtocol {
    implicit val simulationFormat: YamlFormat[Simulation] = yamlFormat2(Simulation)
    implicit val plantFormat: YamlFormat[Plant] = yamlFormat9(Plant)
    implicit val defaultChromosomeDataFormat: YamlFormat[DefaultChromosomeData] = yamlFormat2(DefaultChromosomeData)
    implicit val animalFormat: YamlFormat[Animal] = yamlFormat8(Animal)
    implicit val propertyInfoFormat: YamlFormat[PropertyInfo] = yamlFormat1(PropertyInfo)
    implicit val geneFormat: YamlFormat[Gene] = yamlFormat4(Gene)
    implicit val alleleFormat: YamlFormat[Allele] = yamlFormat6(Allele)
  }

  import CustomYaml._

  override def loadSimulation(configPath: String): CompleteSimulationData = {
    val simulation = loadFileContent(configPath).parseYaml.convertTo[Simulation]
    val animals: Map[CompleteAnimalData, Int] = simulation.animals.map({
      case (k, v) =>
        val animal: CompleteAnimalData = loadAnimal(k)
        val ret: (CompleteAnimalData, Int) = (animal, v)
        ret
    })
    //SimulationData.ttt(animals)
    SimulationBuilder()
        .addAnimals(animals)
        .addPlants(simulation.plants.map({case (k, v) => (loadPlant(k), v)}))
        .buildComplete
  }

  private def loadPlant(path: String): CompletePlantData =
    PlantBuilder().setInfo(loadFileContent(path).parseYaml.convertTo[Plant]).buildComplete

  private def loadAnimal(path: String): CompleteAnimalData = {
    val loadedAnimal = loadFileContent(path).parseYaml.convertTo[Animal]
    val structuralChromosome = loadStructuralChromosome(loadedAnimal.structuralChromosome)
    val regulationChromosome = loadDefaultChromosome(RegulationDefaultGenes.elements, loadedAnimal.regulationChromosome)
    val sexualChromosome = loadDefaultChromosome(SexualDefaultGenes.elements, loadedAnimal.sexualChromosome)
    AnimalBuilder()
      .setName(loadedAnimal.name)
      .setTypology(loadedAnimal.typology)
      .setReign(loadedAnimal.reign)
      .setAlleleLength(loadedAnimal.alleleLength)
      .setGeneLength(loadedAnimal.geneLength)
      .addStructuralChromosome(structuralChromosome)
      .addRegulationChromosome(regulationChromosome)
      .addSexualChromosome(sexualChromosome)
      .buildComplete
  }

  private def loadDefaultChromosome[T <: DefaultGene](genesSet: Set[T], chromosomeData: DefaultChromosomeData): Seq[GeneBuilder[_]] = {
    require(chromosomeData.names.keySet == genesSet.map(_.name))
    val alleles = loadAlleles(chromosomeData.allelesPath)
    //TODO check no wrong alleles
    chromosomeData.names.toSeq.map({
      case (k, v) =>
        GeneBuilder()
          .setDefaultInfo(genesSet.find(e => e.name == k).get)
          .setId(v)
          .addAlleles(alleles.filter(a => a.gene == v))
          //.buildCompleteDefault
    })
  }

  private def loadStructuralChromosome(genesPath: String): Seq[GeneBuilder[_]] =  {
    Folder(genesPath).getFilesAsStream(Folder.YAML)
      .map(loadFileContent(_).parseYaml.convertTo[Gene])
      .map(g =>
      GeneBuilder()
        .setCustomInfo(g)
        .addAlleles(loadAlleles(g.allelesPath))
        //.buildCompleteCustom
      )
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
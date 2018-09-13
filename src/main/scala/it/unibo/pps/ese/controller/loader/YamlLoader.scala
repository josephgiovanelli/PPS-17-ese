package it.unibo.pps.ese.controller.loader

import java.io.InputStream

import it.unibo.pps.ese.controller.loader.beans._
import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
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

  override def loadSimulation(configPath: String): PartialSimulationData = {
    val simulation = loadFileContent(configPath).parseYaml.convertTo[Simulation]
    val animals: Map[PartialAnimalData, Int] = simulation.animals.map({
      case (k, v) =>
        val animal: PartialAnimalData = loadAnimal(k)
        val ret: (PartialAnimalData, Int) = (animal, v)
        ret
    })
    val plants: Map[PartialPlantData, Int] = simulation.plants.map({
      case (k, v) =>
        val ret: (PartialPlantData, Int) = (loadPlant(k), v)
        ret
    })

    SimulationBuilder()
        .addAnimals(animals)
        .addPlants(plants)
        .build
  }

  private def loadPlant(path: String): PartialPlantData = {
    val loadedPlant = loadFileContent(path).parseYaml.convertTo[Plant]
    var builder: PlantBuilder[_] = PlantBuilder()
    if(loadedPlant.name.isValid)
      builder = builder.setName(loadedPlant.name)
    if(loadedPlant.reign.isValid)
      builder = builder.setReign(loadedPlant.reign)
    if(loadedPlant.alleleLength.isValid)
      builder = builder.setAlleleLength(loadedPlant.alleleLength)
    if(loadedPlant.geneLength.isValid)
      builder = builder.setGeneLength(loadedPlant.geneLength)
    if(loadedPlant.attractiveness.isValid)
      builder = builder.setAttractiveness(loadedPlant.attractiveness)
    if(loadedPlant.hardness.isValid)
      builder = builder.setHardness(loadedPlant.hardness)
    if(loadedPlant.availability.isValid)
      builder = builder.setAvailability(loadedPlant.availability)
    if(loadedPlant.height.isValid)
      builder = builder.setHeight(loadedPlant.height)
    if(loadedPlant.nutritionalValue.isValid)
      builder = builder.setNutritionalValue(loadedPlant.nutritionalValue)
    builder.build()
  }

  private def loadAnimal(path: String): PartialAnimalData = {
    val loadedAnimal = loadFileContent(path).parseYaml.convertTo[Animal]
    val structuralChromosome = loadStructuralChromosome(loadedAnimal.structuralChromosome)
    val regulationChromosome = loadDefaultChromosome(RegulationDefaultGenes.elements, loadedAnimal.regulationChromosome)
    val sexualChromosome = loadDefaultChromosome(SexualDefaultGenes.elements, loadedAnimal.sexualChromosome)
    var builder: AnimalBuilder[_] = AnimalBuilder()
    if(loadedAnimal.name.isValid)
      builder = builder.setName(loadedAnimal.name)
    if(loadedAnimal.typology.isValid)
      builder = builder.setTypology(loadedAnimal.typology)
    if(loadedAnimal.reign.isValid)
      builder = builder.setReign(loadedAnimal.reign)
    if(loadedAnimal.alleleLength.isValid)
      builder = builder.setAlleleLength(loadedAnimal.alleleLength)
    if(loadedAnimal.geneLength.isValid)
      builder = builder.setGeneLength(loadedAnimal.geneLength)
    if(structuralChromosome.isValid)
      builder = builder.addStructuralChromosome(structuralChromosome)
    if(regulationChromosome.isValid)
      builder = builder.addRegulationChromosome(regulationChromosome)
    if(sexualChromosome.isValid)
      builder = builder.addSexualChromosome(sexualChromosome)
    builder.build
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

  implicit class ValidableString(str: String) {
    def isValid: Boolean = str != ""
  }

  implicit class ValidableIterable(it: Iterable[_]) {
    def isValid: Boolean = it.nonEmpty
  }

  implicit class ValidableNumeric(num: Double) {
    def isValid: Boolean = num != -1
  }
}
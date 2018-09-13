package it.unibo.pps.ese.controller.loader

import java.io.InputStream

import it.unibo.pps.ese.controller.loader.beans._
import it.unibo.pps.ese.controller.loader.data.AnimalData.{CompleteAnimalData, PartialAnimalData}
import it.unibo.pps.ese.controller.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder._
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

  implicit val int: DefaultValue[Int] = DefaultValue(Integer.MIN_VALUE)
  implicit val double: DefaultValue[Double] = DefaultValue(Double.MinValue)
  implicit val string: DefaultValue[String] = DefaultValue("")
  implicit val iterable: DefaultValue[Iterable[_]] = DefaultValue(Iterable())

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
    //TODO in builder, only check subset here
    //require(chromosomeData.names.keySet == genesSet.map(_.name))
    var alleles: Seq[AlleleBuilder[_]] = Seq()
    if(chromosomeData.allelesPath.isValid) {
      alleles = loadAlleles(chromosomeData.allelesPath)
    }
    //TODO check no wrong alleles
    chromosomeData.names.toSeq.map({
      case (k, v) =>
        var builder: GeneBuilder[_] = GeneBuilder()
          .setDefaultInfo(genesSet.find(e => e.name == k).get)
        if(v.isValid)
          builder = builder.setId(v)
        if(chromosomeData.allelesPath.isValid && v.isValid)
          builder = builder.addAllelesB(alleles.filter(a => a.gene.getOrElse("") == v))
        builder
    })
  }

  private def loadStructuralChromosome(genesPath: String): Seq[GeneBuilder[_]] =  {
    Folder(genesPath).getFilesAsStream(Folder.YAML)
      .map(loadFileContent(_).parseYaml.convertTo[Gene])
      .map(g => {
        var builder: GeneBuilder[_] = GeneBuilder()
        if(g.id.isValid)
          builder = builder.setId(g.id)
        if(g.simpleName.isValid)
          builder = builder.setName(g.simpleName)
        if(g.properties.isValid)
          builder = builder.setCustomProperties(g.properties)
        if(g.allelesPath.isValid)
          builder = builder.addAllelesB(loadAlleles(g.allelesPath))
        builder
      })
  }

  private def loadAlleles(allelesPath: String): Seq[AlleleBuilder[_]] = {
    Folder(allelesPath).getFilesAsStream(Folder.YAML)
      .map(path => {
        val all = loadFileContent(path).parseYaml.convertTo[Allele]
        var builder: AlleleBuilder[_] = AlleleBuilder()
        if(all.id.isValid)
          builder = builder.setId(all.id)
        if(all.gene.isValid)
          builder = builder.setGene(all.gene)
        if(all.consume.isValid)
          builder = builder.setConsume(all.consume)
        if(all.dominance.isValid)
          builder = builder.setDominance(all.dominance)
        if(all.effect.isValid)
          builder = builder.setEffect(all.effect)
        if(all.probability.isValid)
          builder = builder.setProbability(all.probability)
        builder
      })
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

  case class DefaultValue[T](get: T)

  trait Validable[T] {
    def isValid(implicit defaultValue: DefaultValue[T]): Boolean
  }

  implicit class ValidableString(str: String) extends Validable[String] {
    def isValid(implicit defaultValue: DefaultValue[String]): Boolean = str != defaultValue.get
  }

  implicit class ValidableIterable(it: Iterable[_]) extends Validable[Iterable[_]] {
    def isValid(implicit defaultValue: DefaultValue[Iterable[_]]): Boolean = it != defaultValue.get
  }

  implicit class ValidableNumeric(num: Double) extends Validable[Double] {
    def isValid(implicit defaultValue: DefaultValue[Double]): Boolean = num != defaultValue.get
  }

  trait DefaultGet[T] {
    def getOrDefault(implicit defaultValue: DefaultValue[T]): T
  }
}
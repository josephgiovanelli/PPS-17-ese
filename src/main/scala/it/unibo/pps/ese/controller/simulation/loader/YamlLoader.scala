package it.unibo.pps.ese.controller.simulation.loader

import java.io.InputStream

import com.sun.net.httpserver.Authenticator
import it.unibo.pps.ese.controller.simulation.loader.beans._
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.data.builder._
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.{AnimalBuilder, EntityStatus, PlantBuilder}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.{CompleteBuildException, CompleteSimulationBuildException}
import it.unibo.pps.ese.controller.simulation.loader.data.builder.gene.{CustomGeneBuilder, DefaultGeneBuilder}
import it.unibo.pps.ese.controller.simulation.loader.io.File.FileFormats
import it.unibo.pps.ese.controller.simulation.loader.io.{ExistingResource, File, Folder, IOResource}
import it.unibo.pps.ese.utils.DefaultValue
import net.jcazevedo.moultingyaml._

import scala.util.{Failure, Success, Try}


object YamlLoader extends Loader {


  import BeansYamlProtocol._
  import it.unibo.pps.ese.utils.ValidableImplicits.ValidableByDisequality._

  implicit val int: DefaultValue[Int] = DefaultValue(Integer.MIN_VALUE)
  implicit val double: DefaultValue[Double] = DefaultValue(Double.MinValue)
  implicit val string: DefaultValue[String] = DefaultValue("")
  implicit val iterable: DefaultValue[Iterable[_]] = DefaultValue(Iterable())
  implicit def seq[X]: DefaultValue[Seq[X]] = DefaultValue(Seq[X]())

  def loadCompleteSimulation(configFile: File): Try[CompleteSimulationData] = {
    val builder = obtainSimulationBuilder(configFile)
      builder.tryCompleteBuild match {
      case Success(value) =>
        Success(value)
      case Failure(exception) =>
          Failure(exception)
    }
  }

  override def loadSimulation(configFile: File): PartialSimulationData = {
    obtainSimulationBuilder(configFile).build()
  }

  private def obtainSimulationBuilder(configFile: File): SimulationBuilder[_] = {
    val currentFolder = configFile.getParentFolder().get
    val simulation = loadFileContent(configFile).parseYaml.convertTo[Simulation]

    var builder : SimulationBuilder[_] = SimulationBuilder()
    if(simulation.animals.isDefined) {
      val animals = simulation.animals.get.map({
        case (animalConfigPath, v) =>
          (normalizeConfigPath(animalConfigPath, currentFolder) match {case f: File => loadAnimal(f)}, v)
      })
      builder = builder.addAnimals(animals)
    }
    if(simulation.plants.isDefined) {
      val plants = simulation.plants.get.map({
        case (plantConfigPath, v) =>
          (normalizeConfigPath(plantConfigPath, currentFolder) match {case f: File => loadPlant(f)}, v)
      })
      builder = builder.addPlants(plants)
    }
    builder
  }

  private def loadPlant(config: File): PlantBuilder[_ <: EntityStatus] = {
    val loadedPlant = loadFileContent(config).parseYaml.convertTo[Plant]
    var builder: PlantBuilder[_ <: EntityStatus] = PlantBuilder().setName(loadedPlant.name)
    if(loadedPlant.reign.isDefined)
      builder = builder.setReign(loadedPlant.reign.get)
    if(loadedPlant.alleleLength.isDefined)
      builder = builder.setAlleleLength(loadedPlant.alleleLength.get)
    if(loadedPlant.geneLength.isDefined)
      builder = builder.setGeneLength(loadedPlant.geneLength.get)
    if(loadedPlant.hardness.isDefined)
      builder = builder.setHardness(loadedPlant.hardness.get)
    if(loadedPlant.height.isDefined)
      builder = builder.setHeight(loadedPlant.height.get)
    if(loadedPlant.nutritionalValue.isDefined)
      builder = builder.setNutritionalValue(loadedPlant.nutritionalValue.get)
    builder
  }

  private def loadAnimal(config: File): AnimalBuilder[_ <: EntityStatus] = {
    val loadedAnimal = loadFileContent(config).parseYaml.convertTo[Animal]
    var structuralChromosome: Seq[CustomGeneBuilder[_]] = Seq()
    var regulationChromosome: Seq[DefaultGeneBuilder[_]] = Seq()
    var sexualChromosome: Seq[DefaultGeneBuilder[_]] = Seq()
    if(loadedAnimal.structuralChromosome.isDefined)
      structuralChromosome = normalizeConfigPath(loadedAnimal.structuralChromosome.get, config.getParentFolder().get) match {
        case f: Folder =>
          loadStructuralChromosome(f)
      }
    if(loadedAnimal.regulationChromosome.isDefined)
      regulationChromosome = loadDefaultChromosome(RegulationDefaultGenes.elements, loadedAnimal.regulationChromosome.get, config.getParentFolder().get)
    if(loadedAnimal.sexualChromosome.isDefined)
      sexualChromosome = loadDefaultChromosome(SexualDefaultGenes.elements, loadedAnimal.sexualChromosome.get, config.getParentFolder().get)
    var builder: AnimalBuilder[_ <: EntityStatus] = AnimalBuilder().setName(loadedAnimal.name)
    if(loadedAnimal.typology.isDefined)
      builder = builder.setTypology(loadedAnimal.typology.get)
    if(loadedAnimal.reign.isDefined)
      builder = builder.setReign(loadedAnimal.reign.get)
    if(loadedAnimal.alleleLength.isDefined)
      builder = builder.setAlleleLength(loadedAnimal.alleleLength.get)
    if(loadedAnimal.geneLength.isDefined)
      builder = builder.setGeneLength(loadedAnimal.geneLength.get)
    if(structuralChromosome.isValid)
      builder = builder.addStructuralChromosome(structuralChromosome)
    if(regulationChromosome.isValid)
      builder = builder.addRegulationChromosome(regulationChromosome)
    if(sexualChromosome.isValid)
      builder = builder.addSexualChromosome(sexualChromosome)
    builder
  }

  private def loadDefaultChromosome[T <: DefaultGene](genesSet: Set[T], chromosomeData: DefaultChromosomeData,
                                                      currentFolder: Folder): Seq[DefaultGeneBuilder[_]] = {
    //TODO in builder, only check subset here
    //require(chromosomeData.names.keySet == genesSet.map(_.name))
    var alleles: Seq[AlleleBuilder[_]] = Seq()
    if(chromosomeData.allelesPath.isDefined) {
      alleles = normalizeConfigPath(chromosomeData.allelesPath.get, currentFolder) match {
        case f: Folder =>
          loadAlleles(f)
      }
    }
    //TODO check no wrong alleles
    chromosomeData.names.getOrElse(Seq()).toSeq.map({
      case (k, v) =>
        var builder: DefaultGeneBuilder[_] = DefaultGeneBuilder()
          .setDefaultInfo(genesSet.find(e => e.name == k).get)
        if(v.isValid)
          builder = builder.setId(v)
        if(chromosomeData.allelesPath.isDefined && v.isValid)
          builder = builder.addAlleles(alleles.filter(a => a.gene.getOrElse("") == v))
        builder
    })
  }

  private def loadStructuralChromosome(genesFolder: Folder): Seq[CustomGeneBuilder[_]] =  {
    genesFolder.getFilesAsStream(FileFormats.YAML)
      .map(loadFileContent(_).parseYaml.convertTo[Gene])
      .map(g => {
        var builder: CustomGeneBuilder[_] = CustomGeneBuilder().setName(g.simpleName)
        if(g.id.isDefined)
          builder = builder.setId(g.id.get)
        if(g.properties.isDefined)
          builder = builder.setCustomProperties(g.properties.get)
        if(g.allelesPath.isDefined) {
          val alleles = normalizeConfigPath(g.allelesPath.get, genesFolder) match {
            case f: Folder =>
              loadAlleles(f)
          }
          builder = builder.addAlleles(alleles)
        }
        builder
      })
  }

  private def loadAlleles(allelesFolder: Folder): Seq[AlleleBuilder[_]] = {
    allelesFolder.getFilesAsStream(FileFormats.YAML)
      .map(path => {
        val all = loadFileContent(path).parseYaml.convertTo[Allele]
        var builder: AlleleBuilder[_] = AlleleBuilder().setId(all.id)
        if(all.gene.isDefined)
          builder = builder.setGene(all.gene.get)
        if(all.consume.isDefined)
          builder = builder.setConsume(all.consume.get)
        if(all.dominance.isDefined)
          builder = builder.setDominance(all.dominance.get)
        if(all.effect.isDefined)
          builder = builder.setEffect(all.effect.get)
        if(all.probability.isDefined)
          builder = builder.setProbability(all.probability.get)
        builder
      })
  }

  private def normalizeConfigPath(path: String, currentFolder: Folder): ExistingResource = {
    if(path.startsWith("./")) {
      currentFolder.getExistingChildren(path.drop(2)).get
    } else {
      IOResource(path) match {
        case r: ExistingResource =>
          r
        case _ =>
          println(path)
          throw new IllegalStateException()
      }
    }
  }


  private def loadFileContent(file: File): String = {
    loadFileContent(file.openInputStream)
  }

  private def loadFileContent(input: InputStream): String = {
    val source = scala.io.Source.fromInputStream(input)
    val ret = source.mkString
    source.close()
    ret
  }

}
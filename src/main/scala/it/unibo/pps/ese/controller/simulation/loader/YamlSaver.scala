package it.unibo.pps.ese.controller.simulation.loader
import it.unibo.pps.ese.controller.simulation.loader.beans._
import it.unibo.pps.ese.controller.simulation.loader.data.AnimalData.PartialAnimalData
import it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData.PartialCustomGeneData
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.PartialDefaultGeneData
import it.unibo.pps.ese.controller.simulation.loader.data.{PartialAlleleData, PartialPlantData}
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.simulation.loader.io.File.FileFormats
import it.unibo.pps.ese.controller.simulation.loader.io.{ExistingResource, File, FileResource, Folder, FolderResource, IOResource, NotExistingFile, NotExistingFolder, UndefinedNotExistingResource}
import it.unibo.pps.ese.utils.DefaultValue
import net.jcazevedo.moultingyaml._
import java.io.IOException

import it.unibo.pps.ese.controller.simulation.loader.exception.ResourceAlreadyExistsException

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

trait FileSaver extends Saver {
  override type DataSupport = Folder
  def addResourceToOverride(resource: ExistingResource)
}

object YamlSaver {

  def apply(simulationData: PartialSimulationData, simulationName: String): FileSaver =
    new YamlSaverImpl(simulationData, simulationName)

  private class YamlSaverImpl(val simulationData: PartialSimulationData, val simulationName: String) extends FileSaver {

    import BeansYamlProtocol._

    private val fileExtension = FileFormats.YAML.extensions.head
    private var overrideResources: Set[ExistingResource] = Set()
    private val toWrite: ListBuffer[(IOResource, Option[String])] = new ListBuffer()

    implicit val string: DefaultValue[String] = DefaultValue("")
    import it.unibo.pps.ese.utils.DefaultGet._

    def writeFiles(): Try[Unit] = {
      Try(toWrite.foreach({
        case (f: FileResource, Some(str)) =>
          val file = f.getOrCreateFile().getOrElse(throw new IOException())
          file.write(str)
        case (f: FolderResource, _) =>
          f.getOrCreateFolder().getOrElse(throw new IOException())
      })) match {
        case Success(_) =>
          Success(Unit)
        case Failure(exception) =>
          Failure(exception)
      }
    }

    override def saveData(saveLocation: Folder, overrideAll: Boolean): Try[Unit] = {
      val ret = Try(checkFileExistence(saveLocation.getChildren(simulationName + fileExtension), overrideAll, saveMainFile)) match {
        case Success(_) =>
          writeFiles()
        case Failure(exception) =>
          Failure(exception)
      }
      toWrite.clear()
      ret
    }

    def saveMainFile(file: FileResource, overrideAll: Boolean): Unit = {
      val currentFolder = file.getParent().get.getOrCreateFolder().get
      val animals = simulationData.getAnimals.map(iter => {
        iter.map(animalTuple => {
          val fileName = simulationName + "_" + animalTuple._1.name + fileExtension
          checkFileExistence(currentFolder.getChildren(fileName), overrideAll,
            saveAnimal(animalTuple._1, currentFolder))
          (fileName.toRelativePath, animalTuple._2)
        }).toMap
      })
      val plants = simulationData.getPlants.map(iter => {
        iter.map(plantTuple => {
          val fileName = simulationName + "_" + plantTuple._1.name + fileExtension
          checkFileExistence(currentFolder.getChildren(fileName), overrideAll,
            savePlant(plantTuple._1))
          (fileName.toRelativePath, plantTuple._2)
        }).toMap
      })
      val yamlObj = Simulation(animals, plants).toYaml
      (file, Some(yamlObj.prettyPrint)) +=: toWrite
    }

    def saveAnimal(animal: PartialAnimalData, currentFolder: Folder)(file: FileResource, overrideAll: Boolean): Unit = {
      val regulationFolderName: String = simulationName + "_" + animal.name + "_reg"
      val sexualFolderName = simulationName + "_" + animal.name + "_sex"
      var structuralFolderName: Option[String] = None
      var regulationChromosome: Option[DefaultChromosomeData] = None
      var sexualChromosome: Option[DefaultChromosomeData] = None
      if(animal.getRegulationChromosome.isDefined) {
        checkFolderExistence(currentFolder.getChildren(regulationFolderName),
          overrideAll, saveDefaultChromosome(animal.getRegulationChromosome.get))
        regulationChromosome = Some(mapToChromosomeData(animal.getRegulationChromosome.get, regulationFolderName))
      }
      if(animal.getSexualChromosome.isDefined) {
        checkFolderExistence(currentFolder.getChildren(sexualFolderName),
          overrideAll, saveDefaultChromosome(animal.getSexualChromosome.get))
        sexualChromosome = Some(mapToChromosomeData(animal.getSexualChromosome.get, sexualFolderName))
      }
      if(animal.getStructuralChromosome.isDefined) {
        structuralFolderName = Some(simulationName + "_" + animal.name + "_struct")
        checkFolderExistence(currentFolder.getChildren(structuralFolderName.get),
          overrideAll, saveCustomChromosome(animal.getStructuralChromosome.get))
      }
      val yamlObj = Animal(animal.name, animal.getGeneLength, animal.getAlleleLength, animal.getReign,
        animal.getTypology, structuralFolderName.toRelativePath, regulationChromosome, sexualChromosome).toYaml
      (file, Some(yamlObj.prettyPrint)) +=: toWrite
    }

    def mapToChromosomeData(genes: Iterable[PartialDefaultGeneData], allelesPath: String): DefaultChromosomeData = {
      val names = genes.map(gene => (gene.name, gene.getId.getOrDefault)).toMap
      DefaultChromosomeData(Some(allelesPath).toRelativePath, Some(names))
    }

    def saveDefaultChromosome(genes: Iterable[PartialDefaultGeneData])(folder: FolderResource, overrideAll: Boolean): Unit = {
      genes.foreach(gene =>
        if(gene.getAlleles.isDefined) {
          gene.getAlleles.get.foreach(allele => {
            checkFileExistence(folder.getChildren(gene.name + "_" + allele.id + fileExtension), overrideAll, saveAllele(allele))
          })
        }
      )
      (folder, None) +=: toWrite
    }

    def saveCustomChromosome(genes: Iterable[PartialCustomGeneData])(folder: FolderResource, overrideAll: Boolean): Unit = {
      genes.foreach(gene =>
        checkFileExistence(folder.getChildren(gene.name + fileExtension), overrideAll, saveGene(gene, folder))
      )
      (folder, None) +=: toWrite
    }

    def saveGene(gene: PartialCustomGeneData, currentFolder: FolderResource)(file: FileResource, overrideAll: Boolean): Unit = {
      var allelesPath: Option[String] = None
      if(gene.getAlleles.isDefined) {
        allelesPath = Some(gene.name + "_" + "all")
        checkFolderExistence(currentFolder.getChildren(allelesPath.get), overrideAll, saveCustomAlleles(gene.getAlleles.get))
      }
      val properties = gene.getConversionMap.map(content => content.map(t => (t._1, PropertyInfo(t._2))))
      val yamlObj = Gene(gene.getId, gene.name, allelesPath.toRelativePath, properties).toYaml
      (file, Some(yamlObj.prettyPrint)) +=: toWrite
    }

    def saveCustomAlleles(alleles: Iterable[PartialAlleleData])(folder: FolderResource, overrideAll: Boolean): Unit = {
      alleles.foreach(allele => {
        checkFileExistence(folder.getChildren(allele.id + fileExtension), overrideAll, saveAllele(allele))
      })
      (folder, None) +=: toWrite
    }

    def saveAllele(allele: PartialAlleleData)(file: FileResource, overrideAll: Boolean): Unit = {
      val yamlObj = Allele(allele.getGene, allele.id, allele.getDominance, allele.getConsume, allele.getProbability,
        allele.getEffect).toYaml
      (file, Some(yamlObj.prettyPrint)) +=: toWrite
    }

    def savePlant(plant: PartialPlantData)(file: FileResource, overrideAll: Boolean): Unit = {
      val yamlObj = Plant(plant.name, plant.getGeneLength, plant.getAlleleLength, plant.getReign, plant.getHeight,
        plant.getHardness, plant.getNutritionalValue).toYaml
      (file, Some(yamlObj.prettyPrint)) +=: toWrite
    }

    def checkFileExistence(resource: IOResource, overrideAll: Boolean, callback: (FileResource, Boolean) => Unit): Unit = resource match {
      case f: NotExistingFile =>
        callback(f, overrideAll)
      case f: File =>
        if(overrideAll || overrideResources.contains(f)) {
          callback(f, overrideAll)
        } else {
          throw ResourceAlreadyExistsException(f)
        }
    }

    def checkFolderExistence(resource: IOResource, overrideAll: Boolean, callback: (FolderResource, Boolean) => Unit): Unit = resource match {
      case f: NotExistingFolder =>
        callback(f, overrideAll)
      case f: Folder =>
        if(overrideAll || overrideResources.contains(f)) {
          callback(f, overrideAll)
        } else {
          throw ResourceAlreadyExistsException(f)
        }
    }

    implicit class FileString(path: String) {
      def toRelativePath: String = "./" + path
    }

    implicit class FileOptional(opt: Option[String]) {
      def toRelativePath: Option[String] = opt match {
        case Some(path) =>
          Some(path.toRelativePath)
        case None =>
          None
      }
    }

    override def addResourceToOverride(resource: ExistingResource): Unit =
      overrideResources = overrideResources + resource
  }
}

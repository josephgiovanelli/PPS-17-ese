package it.unibo.pps.ese.controller.loader
import it.unibo.pps.ese.controller.loader.beans.Plant
import it.unibo.pps.ese.controller.loader.data.AnimalData.PartialAnimalData
import it.unibo.pps.ese.controller.loader.data.CustomGeneData.PartialCustomGeneData
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.PartialDefaultGeneData
import it.unibo.pps.ese.controller.loader.data.{PartialAlleleData, PartialPlantData}
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.util.io.File.FileFormats
import it.unibo.pps.ese.controller.util.io.{ExistingResource, File, FileResource, Folder, IOResource, NotExistingFile, NotExistingFolder, UndefinedNotExistingResource}

import net.jcazevedo.moultingyaml._

object YamlSaver {

  def apply(simulationData: PartialSimulationData, simulationName: String): Saver =
    new YamlSaverImpl(simulationData, simulationName)

  private class YamlSaverImpl(val simulationData: PartialSimulationData, val simulationName: String) extends Saver {

    import BeansYamlProtocol._

    private val fileExtension = FileFormats.YAML.extensions.head
    private var overrideResources: Set[ExistingResource] = Set()

    override def saveData(saveLocation: Folder, overrideAll: Boolean): Unit = {
      checkFileExistence(saveLocation.getChildren(simulationName + fileExtension), overrideAll, saveMainFile)
    }

    def saveMainFile(file: File, overrideAll: Boolean): Unit = {
      val currentFolder = file.getParentFolder().get
      simulationData.getAnimals.get.map(_._1).foreach(animal => {
        checkFileExistence(currentFolder.getChildren(simulationName + "_" + animal.name + fileExtension), overrideAll,
          saveAnimal(animal))
      })
      simulationData.getPlants.get.map(_._1).foreach(plant => {
        checkFileExistence(currentFolder.getChildren(simulationName + "_" + plant.name + fileExtension), overrideAll,
          savePlant(plant))
      })
      //TODO write main file
    }

    def saveAnimal(animal: PartialAnimalData)(file: File, overrideAll: Boolean): Unit = {
      val currentFolder = file.getParentFolder().get
      val regulationFolderName = simulationName + "_" + animal.name + "_reg"
      val sexualFolderName = simulationName + "_" + animal.name + "_sex"
      val structuralFolderName = simulationName + "_" + animal.name + "_struct"
      if(animal.getRegulationChromosome.isDefined) {
        checkFolderExistence(currentFolder.getChildren(regulationFolderName),
          overrideAll, saveDefaultChromosome(animal.getRegulationChromosome.get))
      }
      if(animal.getSexualChromosome.isDefined) {
        checkFolderExistence(currentFolder.getChildren(sexualFolderName),
          overrideAll, saveDefaultChromosome(animal.getSexualChromosome.get))
      }
      if(animal.getStructuralChromosome.isDefined) {
        checkFolderExistence(currentFolder.getChildren(structuralFolderName),
          overrideAll, saveCustomChromosome(animal.getStructuralChromosome.get))
      }
    }

    def saveDefaultChromosome(genes: Iterable[PartialDefaultGeneData])(folder: Folder, overrideAll: Boolean): Unit = {
      genes.foreach(gene =>
        if(gene.getAlleles.isDefined) {
          gene.getAlleles.get.foreach(allele => {
            checkFileExistence(folder.getChildren(gene.name + "_" + allele.id + fileExtension), overrideAll, saveAllele(allele))
          })
        }
      )
    }

    def saveCustomChromosome(genes: Iterable[PartialCustomGeneData])(folder: Folder, overrideAll: Boolean): Unit = {
      genes.foreach(gene =>
        checkFileExistence(folder.getChildren(gene.name + fileExtension), overrideAll, saveGene(gene))
      )
    }

    def saveGene(gene: PartialCustomGeneData)(file: File, overrideAll: Boolean): Unit = {
      val currentFolder = file.getParentFolder().get
      if(gene.getAlleles.isDefined) {
        checkFolderExistence(currentFolder.getChildren(gene.name + "_" + "all"), overrideAll, saveCustomAlleles(gene.getAlleles.get))
      }
      //TODO write chromosome to file
    }

    def saveCustomAlleles(alleles: Iterable[PartialAlleleData])(folder: Folder, overrideAll: Boolean): Unit = {
      alleles.foreach(allele => {
        checkFileExistence(folder.getChildren(allele.id + fileExtension), overrideAll, saveAllele(allele))
      })
    }

    def saveAllele(animal: PartialAlleleData)(file: File, overrideAll: Boolean): Unit = {
      //TODO write allele to file
      //println("something")
    }

    def savePlant(plant: PartialPlantData)(file: File, overrideAll: Boolean): Unit = {
      val str = Plant(plant.name, plant.getGeneLength, plant.getAlleleLength, plant.getReign, plant.getHeight,
        plant.getAttractiveness, plant.getHardness, plant.getNutritionalValue, plant.getAvailability).toYaml.prettyPrint
      println(str)
    }

    def checkFileExistence(resource: IOResource, overrideAll: Boolean, callback: (File, Boolean) => Unit): Unit = resource match {
      case f: NotExistingFile =>
        callback(f.createFile().getOrElse(throw new IllegalAccessException()), overrideAll)
      case f: File =>
        if(overrideAll || overrideResources.contains(f)) {
          callback(f, overrideAll)
        } else {
          //TODO throw custom exception
        }
    }

    def checkFolderExistence(resource: IOResource, overrideAll: Boolean, callback: (Folder, Boolean) => Unit): Unit = resource match {
      case f: NotExistingFolder =>
        callback(f.createFolder().getOrElse(throw new IllegalAccessException()), overrideAll)
      case f: Folder =>
        if(overrideAll || overrideResources.contains(f)) {
          callback(f, overrideAll)
        } else {
          //TODO throw custom exception
        }
    }

  }
}

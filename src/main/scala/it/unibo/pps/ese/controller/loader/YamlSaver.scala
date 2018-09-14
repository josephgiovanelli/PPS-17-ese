package it.unibo.pps.ese.controller.loader
import it.unibo.pps.ese.controller.loader.data.AnimalData.PartialAnimalData
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.PartialDefaultGeneData
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.util.io.File.FileFormats
import it.unibo.pps.ese.controller.util.io.{ExistingResource, File, FileResource, Folder, IOResource, NotExistingFile, NotExistingFolder, UndefinedNotExistingResource}

object YamlSaver {

  private class YamlSaverImpl(val simulationData: PartialSimulationData, val simulationName: String) extends Saver {

    private val fileExtension = FileFormats.YAML.extensions.head
    private var overrideResources: Set[ExistingResource] = Set()

    override def saveData(saveLocation: Folder, overrideAll: Boolean): Unit = {
      checkFileExistence(saveLocation.getChildren(simulationName + fileExtension), overrideAll, saveMainFile)
    }

    private def saveMainFile(file: File, overrideAll: Boolean): Unit = {
      val currentFolder = file.getParentFolder().get
      simulationData.getAnimals.get.map(_._1).foreach(animal => {
        checkFileExistence(currentFolder.getChildren(simulationName + "_" + animal.name + fileExtension), overrideAll,
          saveAnimal(animal))
      })
      //TODO write file
    }

    def saveAnimal(animal: PartialAnimalData)(file: File, overrideAll: Boolean): Unit = {
      val currentFolder = file.getParentFolder().get
      //if(animal.getRegulationChromosome.isDefined)
        //checkFolderExistence(currentFolder.getChildren(simulationName + "_" + animal.name + "_reg"), overrideAll, saveDefaultChromosome(animal.getRegulationChromosome.get))
    }

    def saveDefaultChromosome(genes: Seq[PartialDefaultGeneData])(file: Folder, overrideAll: Boolean): Unit = {

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

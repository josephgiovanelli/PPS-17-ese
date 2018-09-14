package it.unibo.pps.ese.controller.loader
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.util.io.File.FileFormats
import it.unibo.pps.ese.controller.util.io.{ExistingResource, File, FileResource, Folder, IOResource, NotExistingFile, UndefinedNotExistingResource}

object YamlSaver {

  private class YamlSaverImpl(val simulationData: PartialSimulationData, val simulationName: String) extends Saver {

    private val fileExtension = FileFormats.YAML.extensions.head
    private var overrideResources: Set[ExistingResource] = Set()

    override def saveData(saveLocation: Folder, overrideAll: Boolean): Unit = {
      saveLocation.getChildren(simulationName + fileExtension) match {
        case f: NotExistingFile =>
          saveMainFile(f.createFile().getOrElse(throw new IllegalAccessException()), overrideAll)
        case f: File =>
          if(overrideAll || overrideResources.contains(f)) {
            saveMainFile(f, overrideAll)
          } else {
            //TODO throw custom exception
          }
      }
    }

    private def saveMainFile(file: File, overrideAll: Boolean): Unit = {

    }

  }
}

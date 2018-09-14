package it.unibo.pps.ese.controller.util.io

import java.net.URL

import it.unibo.pps.ese.controller.util.io.File.FileFormat

trait IOResource {
  def getParent(): Option[FolderResource]
}

trait ExistingResource extends IOResource {
  def getParentFolder(): Option[Folder]
}

sealed trait NotExistingResource extends IOResource

trait FolderResource extends IOResource
trait FileResource extends IOResource

sealed trait NotExistingFolder extends NotExistingResource with FolderResource {
  def createFolder(): Option[Folder]
}
sealed trait NotExistingFile extends NotExistingResource with FileResource {
  def createFile(): Option[File]
}

sealed trait UndefinedNotExistingResource extends NotExistingFolder with NotExistingFile {
  def hasFileExtension(format: FileFormat): Boolean
}

object IOResource {

  def apply(path: String): IOResource = {
    apply(new java.io.File(path).toURI.toURL)
  }

  def apply(path: URL): IOResource = {
    val file: java.io.File = new java.io.File(path.toURI)
    if(file.exists()) {
      if(file.isFile) {
        File(path)
      } else {
        Folder(path)
      }
    } else {
      new UndefinedNotExistingResourceImpl(path)
    }
  }

  private class UndefinedNotExistingResourceImpl(path: URL) extends IOResourceImpl(path) with UndefinedNotExistingResource {
    require(!javaFile.exists())
    override def createFile(): Option[File] = {
      javaFile.createNewFile()
      Some(File(javaFile))
    }

    override def createFolder(): Option[Folder] = {
      javaFile.mkdir()
      Some(Folder(javaFile))
    }

    override def hasFileExtension(format: FileFormat): Boolean = format.extensions.exists(ext => javaFile.getName.endsWith(ext))
  }
}

abstract class IOResourceImpl(path: URL) extends IOResource {
  protected val url: URL = path
  require(url != null)
  protected val javaFile: java.io.File = new java.io.File(url.toURI)

  def getParent(): Option[FolderResource] = {
    if(javaFile.getParent == null)
      None
    else
      IOResource(javaFile.getParent) match {
        case r: FolderResource =>
          Some(r)
        case _ =>
          throw new IllegalStateException()
      }
  }
}

abstract class ExistingResourceImpl(path: URL) extends IOResourceImpl(path) with ExistingResource {

  override def getParentFolder(): Option[Folder] = {
    getParent() match {
      case r: Folder =>
        Some(r)
      case _ =>
        throw new IllegalStateException()
    }
  }
}
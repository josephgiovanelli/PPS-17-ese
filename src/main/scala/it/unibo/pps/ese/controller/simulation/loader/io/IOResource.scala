package it.unibo.pps.ese.controller.simulation.loader.io

import java.net.URL

import it.unibo.pps.ese.controller.simulation.loader.io.File.FileFormat
import org.apache.commons.io.FilenameUtils

trait IOResource {
  def getParent: Option[FolderResource]
  def getName: String
}

trait ExistingResource extends IOResource {
  def getParentFolder: Option[Folder]
}

trait FolderResource extends IOResource {
  def getOrCreateFolder(): Option[Folder] = this match {
    case f: NotExistingFolder =>
      f.createFolder()
    case f: Folder =>
      Some(f)
  }

  def getChildren(relativePath: String): IOResource
}
trait FileResource extends IOResource {
  def getOrCreateFile(): Option[File] = this match {
    case f: NotExistingFile =>
      f.createFile()
    case f: File =>
      Some(f)
  }
}

sealed trait NotExistingResource extends IOResource

sealed trait NotExistingFolder extends NotExistingResource with FolderResource {
  def createFolder(): Option[Folder]
}
sealed trait NotExistingFile extends NotExistingResource with FileResource {
  def createFile(): Option[File]
}

sealed trait UndefinedNotExistingResource extends NotExistingFolder with NotExistingFile {
  def hasFileExtension(format: FileFormat): Boolean
}

sealed abstract class IOResourceImpl(path: URL) extends IOResource {
  protected val url: URL = path
  require(url != null)
  protected val javaFile: java.io.File = new java.io.File(url.toURI)

  def getParent: Option[FolderResource] = {
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

  def getName: String = javaFile.getName

  def canEqual(other: Any): Boolean = other.isInstanceOf[IOResourceImpl]

  override def equals(other: Any): Boolean = other match {
    case that: IOResourceImpl =>
      (that canEqual this) &&
        url.equals(that.url)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(url)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = url.toString
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

  private[this] class UndefinedNotExistingResourceImpl(path: URL) extends IOResourceImpl(path) with UndefinedNotExistingResource {
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

    override def getChildren(relativePath: String): IOResource =
      IOResource(FilenameUtils.concat(javaFile.getAbsolutePath, relativePath))
  }
}

abstract class ExistingResourceImpl(path: URL) extends IOResourceImpl(path) with ExistingResource {
  require(javaFile.exists())

  override def getParentFolder: Option[Folder] = {
    getParent match {
      case Some(r: Folder) =>
        Some(r)
      case _ =>
        throw new IllegalStateException()
    }
  }
}
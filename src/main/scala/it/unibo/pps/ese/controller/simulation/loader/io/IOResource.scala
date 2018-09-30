package it.unibo.pps.ese.controller.simulation.loader.io

import java.net.URL

import it.unibo.pps.ese.controller.simulation.loader.io.File.FileFormat
import org.apache.commons.io.FilenameUtils

/** Trait that defines a generic IO resource*/
trait IOResource {
  /**
    * @return Parent. It is a folder, that can exists or not
    */
  def getParent: Option[FolderResource]

  /**
    * @return Resource name
    */
  def getName: String
}

/** Trait that defines a generic IO resource that already exists*/
trait ExistingResource extends IOResource {
  /**
    * @return Java file corresponding to this resource
    */
  def rawFile: java.io.File


  /**
    * @return Parent folder
    */
  def getParentFolder: Option[Folder]
}

/** Trait that defines a generic IO resource  that not exists*/
sealed trait NotExistingResource extends IOResource

/** Trait that defines a folder IO resource, that can exist or not*/
trait FolderResource extends IOResource {
  /** Method returns existing folder corresponding to this folder resource. If the folder not exists, it is created.
    * The creation process fails if the resource needs super folders creation
    *
    * @return Required folder
    */
  def getOrCreateFolder(): Option[Folder] = this match {
    case f: NotExistingFolder =>
      f.createFolder()
    case f: Folder =>
      Some(f)
  }

  /** Obtain children resource with relative path starting from this folder as input
    *
    * @param relativePath Relative path to required children
    * @return Children IOResource that can be matched to check children status
    */
  def getChildren(relativePath: String): IOResource
}

/** Trait that defines a file IO resource, that can exist or not*/
trait FileResource extends IOResource {

  /** Method returns existing file corresponding to this folder resource. If the file not exists, it is created.
    * The creation process fails if the resource needs super folders creation
    *
    * @return Required file
    */
  def getOrCreateFile(): Option[File] = this match {
    case f: NotExistingFile =>
      f.createFile()
    case f: File =>
      Some(f)
  }
}

/** Trait that defines a folder that not already exists*/
sealed trait NotExistingFolder extends NotExistingResource with FolderResource {
  /** Method creates not existing folder. The process fails if the resource needs super folders
    * creation
    *
    * @return Created folder
    */
  def createFolder(): Option[Folder]
}

/** Trait that defines a file that not already exists*/
sealed trait NotExistingFile extends NotExistingResource with FileResource {
  /** Method creates not existing file. The process fails if the resource needs super folders
    * creation
    *
    * @return Created file
    */
  def createFile(): Option[File]
}

/** Trait that defines an undefined IO resource that not already exists*/
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

/** Factory object to create IO resources starting from various representations*/
object IOResource {

  /** Creates the IO resource corresponding to given representation. Result can be matched to identify resource status
    *
    * @param path Resource's path
    * @return Resource
    */
  def apply(path: String): IOResource = {
    apply(new java.io.File(path).toURI.toURL)
  }

  /** Creates the IO resource corresponding to given representation. Result can be matched to identify resource status
    *
    * @param path Resource's URL
    * @return Resource
    */
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
  val rawFile: java.io.File = javaFile

  override def getParentFolder: Option[Folder] = {
    getParent match {
      case Some(r: Folder) =>
        Some(r)
      case _ =>
        throw new IllegalStateException()
    }
  }
}
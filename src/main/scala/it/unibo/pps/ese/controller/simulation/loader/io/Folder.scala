package it.unibo.pps.ese.controller.simulation.loader.io

import java.io
import java.io.InputStream
import java.net.URL

import org.apache.commons.io.FilenameUtils

import it.unibo.pps.ese.controller.simulation.loader.io.File.FileFormat
import org.apache.commons.io.FileUtils

/** Trait that defines a folder that already exists*/
trait Folder extends ExistingResource with FolderResource {

  /**
    * @param fileFormats Optional file formats used as filter
    * @return Files contained in the folder
    */
  def getFiles(fileFormats: FileFormat*): Seq[File]

  /**
    * @param fileFormats Optional file formats used as filter
    * @return Files contained in the folder as InputStream
    */
  def getFilesAsStream(fileFormats: FileFormat*): Seq[InputStream]

  /** Obtain children resource with relative path starting from this folder as input
    *
    * @param relativePath Relative path to required children
    * @return An option containing children if exists
    */
  def getExistingChildren(relativePath: String): Option[ExistingResource]
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.io.Folder]]*/
object Folder {

  /** Creates [[it.unibo.pps.ese.controller.simulation.loader.io.Folder]] starting from its path as URL
    *
    * @param folderPath Folder's URL
    * @return Required Folder
    */
  def apply(folderPath: URL): Folder = new FolderImpl(folderPath)

  /** Creates [[it.unibo.pps.ese.controller.simulation.loader.io.Folder]] starting from its path as String
    *
    * @param folderPath Folder's path
    * @return Required Folder
    */
  def apply(folderPath: String): Folder = Folder(new io.File(folderPath))

  /** Creates [[it.unibo.pps.ese.controller.simulation.loader.io.Folder]] starting from a java File
    *
    * @param file Java file
    * @return Required Folder
    */
  def apply(file: java.io.File): Folder = new FolderImpl(file.toURI.toURL)

  private[this] class FolderImpl(folderPath: URL) extends ExistingResourceImpl(folderPath) with Folder {
    require(javaFile.isDirectory)

    def getFiles: Seq[File] = javaFile.listFiles().filter(_.isFile).map(File(_)).toSeq

    def getFiles(fileFormats: FileFormat*): Seq[File] = {
      var files = javaFile.listFiles().filter(_.isFile).map(File(_))
      if(fileFormats.nonEmpty) {
        files = files.filter(f => fileFormats.flatMap(_.extensions).exists(ext => f.name.endsWith(ext)))
      }
      files.toSeq
    }

    def getFilesAsStream(fileFormats: FileFormat*): Seq[InputStream] = convertToInputStream(getFiles(fileFormats:_*))

    private def convertToInputStream(urls: Seq[File]): Seq[InputStream] = urls.map(_.rawFile).map(FileUtils.openInputStream)

    override def getExistingChildren(relativePath: String): Option[ExistingResource] = {
      IOResource(FilenameUtils.concat(javaFile.getAbsolutePath, relativePath)) match {
        case r: ExistingResource =>
          Some(r)
        case _ =>
          None
      }
    }

    override def getChildren(relativePath: String): IOResource =
      IOResource(FilenameUtils.concat(javaFile.getAbsolutePath, relativePath))
  }
}



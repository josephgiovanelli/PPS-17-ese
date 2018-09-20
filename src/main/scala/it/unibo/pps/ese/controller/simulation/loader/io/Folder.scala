package it.unibo.pps.ese.controller.simulation.loader.io

import java.io
import java.io.InputStream
import java.net.URL

import org.apache.commons.io.FilenameUtils

import it.unibo.pps.ese.controller.simulation.loader.io.File.FileFormat
import org.apache.commons.io.FileUtils

trait Folder extends ExistingResource with FolderResource {
  def getFiles: Seq[File]
  def getFiles(fileFormat: FileFormat): Seq[File]
  def getFilesAsStream: Seq[InputStream]
  def getFilesAsStream(fileFormat: FileFormat): Seq[InputStream]
  def getExistingChildren(relativePath: String): Option[ExistingResource]
}

object Folder {

  def apply(folderPath: URL): Folder = new FolderImpl(folderPath)
  def apply(folderPath: String): Folder = Folder(new io.File(folderPath))
  def apply(file: java.io.File): Folder = new FolderImpl(file.toURI.toURL)

  private class FolderImpl(folderPath: URL) extends ExistingResourceImpl(folderPath) with Folder {
    require(javaFile.isDirectory)

    def getFiles: Seq[File] = javaFile.listFiles().filter(_.isFile).map(File(_)).toSeq

    def getFiles(fileFormat: FileFormat): Seq[File] = getFiles.filter(f => fileFormat.extensions.exists(ext => f.name.endsWith(ext)))

    def getFilesAsStream: Seq[InputStream] = convertToInputStream(getFiles)

    def getFilesAsStream(fileFormat: FileFormat): Seq[InputStream] = convertToInputStream(getFiles(fileFormat))

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



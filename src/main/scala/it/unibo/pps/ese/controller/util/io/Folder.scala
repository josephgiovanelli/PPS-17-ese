package it.unibo.pps.ese.controller.util.io

import java.io
import java.io.{File, InputStream}
import java.net.URL

import it.unibo.pps.ese.controller.util.io.Folder.FileFormat
import org.apache.commons.io.FileUtils
import org.kaikikm.threadresloader.ResourceLoader

trait Folder {
  def getFiles: Seq[File]
  def getFiles(fileFormat: FileFormat): Seq[File]
  def getFilesAsStream: Seq[InputStream]
  def getFilesAsStream(fileFormat: FileFormat): Seq[InputStream]
  def getOrCreateFile(): Option[File]
  def getOrCreateFolder(): Option[Folder]
}

object Folder {

  def apply(folderPath: URL): Folder = new FolderImpl(folderPath)
  def apply(folderPath: String): Folder = new FolderImpl(new io.File(folderPath).toURI.toURL)

  sealed abstract class FileFormat(val extensions: Seq[String])
  case object YAML extends FileFormat(Seq(".yml", ".yaml"))

  private class FolderImpl(folderPath: URL) extends IOResourceImpl(folderPath) with Folder {
    require(javaFile.isDirectory)

    def getFiles: Seq[File] = javaFile.listFiles().filter(_.isFile).map(File(_)).toSeq

    def getFiles(fileFormat: FileFormat): Seq[File] = getFiles.filter(f => fileFormat.extensions.map(ext => f.name.endsWith(ext)).exists(b => b))

    def getFilesAsStream: Seq[InputStream] = convertToInputStream(getFiles)

    def getFilesAsStream(fileFormat: FileFormat): Seq[InputStream] = convertToInputStream(getFiles(fileFormat))

    private def convertToInputStream(urls: Seq[File]): Seq[InputStream] = urls.map(_.rawFile).map(FileUtils.openInputStream)

    override def getOrCreateFile(): Option[File] = ???

    override def getOrCreateFolder(): Option[Folder] = ???
  }
}



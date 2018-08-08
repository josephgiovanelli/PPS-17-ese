package it.unibo.pps.ese.controller.util.io

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
}

object Folder {

  def apply(folderPath: String): Folder = new FolderImpl(folderPath)

  sealed abstract class FileFormat(val extensions: Seq[String])
  case object YAML extends FileFormat(Seq(".yml", ".yaml"))

  private class FolderImpl(folderPath: String) extends Folder {
    val url: URL = ResourceLoader.getResource(folderPath)
    require(url != null)
    val folder: File = new File(url.toURI)
    require(folder.isDirectory)

    def getFiles: Seq[File] = folder.listFiles().filter(_.isFile).toSeq

    def getFiles(fileFormat: FileFormat): Seq[File] = getFiles.filter(_.getName.endsWith(fileFormat.extensions))

    def getFilesAsStream: Seq[InputStream] = convertToInputStream(getFiles)

    def getFilesAsStream(fileFormat: FileFormat): Seq[InputStream] = convertToInputStream(getFiles(fileFormat))

    private def convertToInputStream(urls: Seq[File]): Seq[InputStream] = urls.map(FileUtils.openInputStream)

  }
}



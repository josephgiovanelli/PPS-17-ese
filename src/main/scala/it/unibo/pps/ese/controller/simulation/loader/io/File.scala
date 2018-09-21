package it.unibo.pps.ese.controller.simulation.loader.io

import java.io.InputStream
import java.net.URL
import java.nio.charset.StandardCharsets

import org.apache.commons.io.FileUtils


trait File extends ExistingResource with FileResource {
  def rawFile: java.io.File
  def name: String
  def openInputStream: InputStream
  def write(content: String)
  def append(content: String)
}

object File {

  sealed abstract class FileFormat(val extensions: Seq[String])
  object FileFormats {
    case object YAML extends FileFormat(Seq(".yml", ".yaml"))
  }

  def apply(filePath: URL): File = new FileImpl(filePath)
  def apply(filePath: String): File = new FileImpl(new java.io.File(filePath).toURI.toURL)
  def apply(file: java.io.File): File = new FileImpl(file.toURI.toURL)

  private class FileImpl(filePath: URL) extends ExistingResourceImpl(filePath) with File {
    val rawFile: java.io.File = javaFile
    val name: String = javaFile.getName
    require(javaFile.isFile)

    override def openInputStream: InputStream = FileUtils.openInputStream(javaFile)

    override def write(content: String): Unit = {
      FileUtils.writeStringToFile(this, content, StandardCharsets.UTF_8, false)
    }

    override def append(content: String): Unit = {
      FileUtils.writeStringToFile(this, content, StandardCharsets.UTF_8, true)
    }
  }
}
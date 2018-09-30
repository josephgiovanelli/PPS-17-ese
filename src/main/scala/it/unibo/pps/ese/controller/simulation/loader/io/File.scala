package it.unibo.pps.ese.controller.simulation.loader.io

import java.io.InputStream
import java.net.URL
import java.nio.charset.StandardCharsets

import org.apache.commons.io.FileUtils

/** Trait that defines a file that already exists*/
trait File extends ExistingResource with FileResource {

  /**
    * @return File name
    */
  def name: String

  /**
    * @return File's input stream
    */
  def openInputStream: InputStream

  /** The method writes text to file. Old content of file is overwritten
    *
    * @param content Text to write
    */
  def write(content: String)

  /** The method appends text to file
    *
    * @param content Text to write
    */
  def append(content: String)
}

/** Factory object for [[it.unibo.pps.ese.controller.simulation.loader.io.File]]*/
object File {

  /** Abstract class representing a file format*/
  abstract class FileFormat(val extensions: Seq[String])
  /** Object containing supported file extensions*/
  object FileFormats {
    case object YAML extends FileFormat(Seq(".yml", ".yaml"))
  }

  /** Creates [[it.unibo.pps.ese.controller.simulation.loader.io.File]] starting from its path as URL
    *
    * @param filePath File's URL
    * @return Required File
    */
  def apply(filePath: URL): File = new FileImpl(filePath)

  /** Creates [[it.unibo.pps.ese.controller.simulation.loader.io.File]] starting from its path as String
    *
    * @param filePath File's path as String
    * @return Required File
    */
  def apply(filePath: String): File = new FileImpl(new java.io.File(filePath).toURI.toURL)

  /** Creates [[it.unibo.pps.ese.controller.simulation.loader.io.File]] starting from a java file
    *
    * @param file Java file
    * @return Required File
    */
  def apply(file: java.io.File): File = new FileImpl(file.toURI.toURL)

  private class FileImpl(filePath: URL) extends ExistingResourceImpl(filePath) with File {
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
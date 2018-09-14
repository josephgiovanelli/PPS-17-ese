package it.unibo.pps.ese.controller.util.io

import java.net.URL


trait File extends ExistingResource with FileResource {
  def rawFile: java.io.File
  def name: String
}

object File {

  def apply(filePath: URL): File = new FileImpl(filePath)
  def apply(file: java.io.File): File = new FileImpl(file.toURI.toURL)

  private class FileImpl(filePath: URL) extends ExistingResourceImpl(filePath) with File {
    val rawFile: java.io.File = javaFile
    val name: String = javaFile.getName
  }
}
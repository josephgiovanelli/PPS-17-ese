package it.unibo.pps.ese.controller.simulation.loader

package object io {
  implicit def myFileToJavaFile(file: File): java.io.File = file.rawFile
}

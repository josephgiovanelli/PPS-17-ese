package it.unibo.pps.ese.controller.util

package object io {
  implicit def myFileToJavaFile(file: File): java.io.File = file.rawFile
}

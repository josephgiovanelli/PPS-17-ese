package it.unibo.pps.ese.controller.util.io

import java.net.URL

trait IOResource {
  def getParent(): Option[Folder]
}

abstract class IOResourceImpl(path: URL) extends IOResource {
  protected val url: URL = path
  require(url != null)
  protected val javaFile: java.io.File = new java.io.File(url.toURI)

  def getParent(): Option[Folder] = {
    if(javaFile.getParent == null)
      None
    else
      Some(Folder(javaFile.getParent))
  }
}
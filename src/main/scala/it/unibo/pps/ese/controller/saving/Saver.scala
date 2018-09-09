package it.unibo.pps.ese.controller.saving

trait Saver {
  def saveState()
}

object Saver extends Saver {

  val basePath: String = "./src/main/resources/saves"

  override def saveState(): Unit = {
    if (Memento.isDefined) {
      import Memento._


    } else {
      throw new IllegalStateException("Some data missing")
    }
  }
}

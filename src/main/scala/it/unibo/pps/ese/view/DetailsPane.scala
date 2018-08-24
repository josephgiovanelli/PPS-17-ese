package it.unibo.pps.ese.view

import scalafx.scene.control.{Label, ScrollPane}
import scalafx.scene.layout.BorderPane

trait DetailsPane extends ScrollPane {

  def showDetails(e: Entity): Unit
  def clearDetails() : Unit
}

object DetailsPane {
  def apply(): DetailsPane = new DetailsPaneImpl()
}

class DetailsPaneImpl extends DetailsPane {

  val nameLabel = Label("")
  val mainPane = new BorderPane()
  mainPane.center = nameLabel

  content = mainPane

  override def showDetails(e: Entity): Unit = {
    nameLabel.text = e.name
  }

  override def clearDetails(): Unit = {
    nameLabel.text = ""
  }
}

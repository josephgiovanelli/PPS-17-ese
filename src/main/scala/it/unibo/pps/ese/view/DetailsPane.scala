package it.unibo.pps.ese.view

import scalafx.scene.control.{Button, Label, ScrollPane}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.BorderPane
import scalafx.Includes._

trait DetailsPane extends ScrollPane {

  def showDetails(e: Entity): Unit
  def clearDetails() : Unit
}

object DetailsPane {
  def apply(mainComponent: MainComponent): DetailsPane = new DetailsPaneImpl(mainComponent)
}

class DetailsPaneImpl(mainComponent: MainComponent) extends DetailsPane {

  val nameLabel = Label("")
  val mainPane = new BorderPane()
  mainPane.center = nameLabel
  val button:Button = new Button("Genome")
  mainPane.bottom = button
//  button.onMouseClicked = (me:MouseEvent) => {
//    mainComponent.setScene(ViewType.GenomeView)
//  }
  content = mainPane

  override def showDetails(e: Entity): Unit = {
    nameLabel.text = e.name
  }

  override def clearDetails(): Unit = {
    nameLabel.text = ""
  }
}

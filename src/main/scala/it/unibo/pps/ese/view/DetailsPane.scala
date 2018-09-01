package it.unibo.pps.ese.view

import it.unibo.pps.ese.genetics.entities.AnimalInfo
import it.unibo.pps.ese.view.speciesdetails.QualityViewerBox
import scalafx.scene.control.{Button, Label, ProgressBar, ScrollPane}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.Includes._
import scalafx.geometry.Pos

trait DetailsPane extends ScrollPane {

  def showDetails(e: Entity,animalInfo:AnimalInfo): Unit
  def clearDetails() : Unit
}

object DetailsPane {
  def apply(mainComponent: MainComponent): DetailsPane = new DetailsPaneImpl(mainComponent)
}

class DetailsPaneImpl(mainComponent: MainComponent) extends DetailsPane {

  val nameLabel = Label("")
  val mainPane = new BorderPane()
  val button:Button = new Button("Genome")
//  mainPane.bottom = button
  val vBox:VBox = new VBox()
  vBox.spacing = 10
  mainPane.center = vBox

  content = mainPane

  override def showDetails(e: Entity,animalInfo: AnimalInfo): Unit = {
    nameLabel.text = e.name
    vBox.children = nameLabel :: animalInfo.qualities
      .values.map(q=> new QualityViewerBox(q.qualityType.toString,q.qualityValue)).toList
  }

  override def clearDetails(): Unit = {
    nameLabel.text = ""
  }

}

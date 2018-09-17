package it.unibo.pps.ese.view

import it.unibo.pps.ese.entitybehaviors.LifePhases
import it.unibo.pps.ese.genericworld.model.{EntityInfo, EntityState}
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, Carnivorous, Female, Herbivore, Male, PlantInfo}
import it.unibo.pps.ese.view.speciesdetails.{NonNumericQualityViewerBox, QualityViewerBox}
import scalafx.scene.control.{Button, Label, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._
import it.unibo.pps.ese.view.speciesdetails.QualityBoxUtilities._
import it.unibo.pps.ese.view.utilities.TextUtilities._
import it.unibo.pps.ese.view.utilities.EntityConversions._
trait DetailsPane extends ScrollPane {

  def showDetails(e: EntityState): Unit
  def clearDetails() : Unit
}

object DetailsPane {
  def apply(mainComponent: MainComponent): DetailsPane = new DetailsPaneImpl(mainComponent)
}

class DetailsPaneImpl(mainComponent: MainComponent) extends DetailsPane {

  val nameLabel = Label("")
  val mainPane = new BorderPane()
  mainPane.translateY = 5

  val title:HBox = "Entity Details".toHBox
  title.prefWidth <==width
  mainPane.top = title
  val button:Button = new Button("Genome")
  val vBox:VBox = new VBox()
  vBox.translateY = 10
  vBox.spacing = 10
  mainPane.center = vBox

  content = mainPane

  override def showDetails(e: EntityState): Unit = e.state.baseEntityInfo match {
    case AnimalInfo(species,gender,dietType,_,qualities,_) =>
      nameLabel.text = e.state.species.toString
      val genderColor = gender match {
        case Male => "-fx-accent: cyan;"
        case Female => "-fx-accent: pink;"
      }
      val dietColor = dietType match {
        case Herbivore => "-fx-accent: green;"
        case Carnivorous => "-fx-accent: red"
      }

      val lifePhaseBox = e.state.lifePhase match {
        case LifePhases.CHILD =>
          new NonNumericQualityViewerBox("Child","-fx-accent: lightGreen;")
        case LifePhases.ADULT =>
          new NonNumericQualityViewerBox("Adult","-fx-accent: red;")
        case LifePhases.ELDERLY =>
          new NonNumericQualityViewerBox("Elderly","-fx-accent: grey;")
      }
      val reignBox = new NonNumericQualityViewerBox("Animal","-fx-accent: orange;")
      val genderBox = new NonNumericQualityViewerBox(gender.toString,genderColor)
      val dietBox = new NonNumericQualityViewerBox(dietType.toString,dietColor)
      vBox.children =
        nameLabel ::
        reignBox::
        genderBox ::
        dietBox ::
        lifePhaseBox::
        e.state.numericQualities
          .map(q=>q._1--->q._2).toList

    case PlantInfo(s,g,q) =>
      val reignBox = new NonNumericQualityViewerBox("Plant","-fx-accent: green;")
      nameLabel.text = e.state.species.toString
      vBox.children =
        nameLabel ::
        reignBox::
        e.state.numericQualities
          .map(q=>q._1--->q._2).toList

  }
  override def clearDetails(): Unit = {
    vBox.children clear()
  }

}

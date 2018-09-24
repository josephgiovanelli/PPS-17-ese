package it.unibo.pps.ese.view.sections.statistics

import it.unibo.pps.ese.model.dataminer.{AnimalDynamicData, EntityTimedRecord}
import it.unibo.pps.ese.model.components.animals.LifePhases
import it.unibo.pps.ese.view.sections.genome.NonNumericQualityViewerBox

import scalafx.scene.control.{Label, ScrollPane, TextArea}
import scalafx.scene.layout.{BorderPane, VBox}
import it.unibo.pps.ese.view.sections.genome.QualityBoxUtilities._

import scalafx.geometry.Insets

class DetailsReplayPane() extends ScrollPane {

  val nameLabel = Label("")
  val positionLabel = Label("")
  val eraLabel = Label("")
  val actions = new TextArea()
  actions prefWidth = 350
  actions.setWrapText(true)
  val mainPane = new BorderPane()
  val vBox:VBox = new VBox()
  vBox.spacing = 10
  vBox.margin = Insets(0, 0, 10, 0)
  mainPane.center = vBox
  mainPane.bottom = actions

  content = mainPane

  def showDetails(target: EntityTimedRecord): Unit = {

    target.dynamicData match {
      case animalDynamic: AnimalDynamicData =>
        nameLabel.text = "Species: " + target.structuralData.species
        positionLabel.text = "Position: " + target.dynamicData.position.toString
        eraLabel.text = "Era: " + target.era
        val genderColor = target.structuralData.gender match {
          case "male" => "-fx-accent: cyan;"
          case "female" => "-fx-accent: pink;"
          case _ => "-fx-accent: black;"
        }
        val lifePhaseBox = animalDynamic.lifePhase match {
          case "CHILD" =>
            new NonNumericQualityViewerBox("Child","-fx-accent: lightGreen;")
          case "ADULT" =>
            new NonNumericQualityViewerBox("Adult","-fx-accent: red;")
          case "ELDERLY" =>
            new NonNumericQualityViewerBox("Elderly","-fx-accent: grey;")
        }
        val reignBox = new NonNumericQualityViewerBox("Animal","-fx-accent: orange;")
        val genderBox = new NonNumericQualityViewerBox(target.structuralData.gender.toString,genderColor)
        vBox.children =
          nameLabel ::
          eraLabel ::
          positionLabel ::
          reignBox::
          genderBox ::
          lifePhaseBox::
          getAllAnimalQualities(animalDynamic)
            .map(q=>q._1--->q._2).toList
    }

    def getAllAnimalQualities(entityDetails:AnimalDynamicData):Seq[(String,Double)] = {
      "Age"->entityDetails.age.toDouble::
      "Speed"->entityDetails.speed::
      "Energy"->entityDetails.energy::
      "Nutritional Value"->entityDetails.nutritionalValue::
      List()
    }
  }

  def logInteraction(action: String, target: String, era: Long): Unit =
    actions.appendText("Target entity " + action + " entity " + target + " in era " + era + ".\n")
}

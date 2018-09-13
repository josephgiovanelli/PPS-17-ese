package it.unibo.pps.ese.view

import it.unibo.pps.ese.entitybehaviors.LifePhases
import it.unibo.pps.ese.genericworld.model.EntityInfo
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, Carnivorous, Female, Herbivore, Male, PlantInfo}
import it.unibo.pps.ese.view.speciesdetails.{NonNumericQualityViewerBox, QualityViewerBox}
import scalafx.scene.control.{Button, Label, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._
import it.unibo.pps.ese.view.speciesdetails.QualityBoxUtilities._
import it.unibo.pps.ese.view.utilities.TextUtilities._

trait DetailsPane extends ScrollPane {

  def showDetails(e: Entity,entityDetails: EntityInfo): Unit
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

  override def showDetails(e: Entity,entityDetails: EntityInfo): Unit = entityDetails.baseEntityInfo match {
    case AnimalInfo(species,gender,dietType,_,qualities,_) =>
      nameLabel.text = e.name
      val genderColor = gender match {
        case Male => "-fx-accent: cyan;"
        case Female => "-fx-accent: pink;"
      }
      val dietColor = dietType match {
        case Herbivore => "-fx-accent: green;"
        case Carnivorous => "-fx-accent: red"
      }

      val lifePhaseBox = entityDetails.lifePhase match {
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
      vBox.children = nameLabel ::
        reignBox::
        genderBox ::
        dietBox ::
        lifePhaseBox::
        getAllAnimalQualities(entityDetails)
          .map(q=>q._1--->q._2).toList
    case PlantInfo(s,g,q) =>
      val reignBox = new NonNumericQualityViewerBox("Plant","-fx-accent: green;")
      nameLabel.text = e.name
      vBox.children = nameLabel ::
        reignBox::
        getAllPlantQualities(entityDetails)
          .map(q=>q._1--->q._2).toList

  }
  def getAllAnimalQualities(entityDetails:EntityInfo):Seq[(String,Double)] = {
    "Strong"->entityDetails.strong::
    "Action Field"->entityDetails.actionField::
    "Visual Field"->entityDetails.visualField::
    "Attractiveness"->entityDetails.attractiveness::
    "Speed"->entityDetails.actualSpeed::
    "Fertility"->entityDetails.fertility::
    "Age"->entityDetails.age.toDouble::
    "Average Life"->entityDetails.averageLife::
    "Percentage Decay"->entityDetails.percentageDecay::
    "Energy"->entityDetails.energy::
    "Energy Requirement"->entityDetails.energyRequirements::
    "Height"->entityDetails.height::
    "Nutritional Value"->entityDetails.nutritionalValue::
    "Defense"->entityDetails.defense::
    List()
  }
  def getAllPlantQualities(entityDetails:EntityInfo):Seq[(String,Double)] = {
    "Height"->entityDetails.height::
    "Nutritional Value"->entityDetails.nutritionalValue::
//    "Attractiveness"->entityDetails.attractiveness::
//    "Hardness"->entityDetails.strong::
    "Availability"->entityDetails.availability::
    List()
  }
  override def clearDetails(): Unit = {
    vBox.children clear()
  }

}

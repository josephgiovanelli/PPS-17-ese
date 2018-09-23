package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal

import it.unibo.pps.ese.view.sections.configuration.visualization.panes._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{ErrorLabel, WhiteLabel}
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.EntitiesInfo
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals.AnimalBaseInfo
import it.unibo.pps.ese.view.sections.configuration.visualization.core._

import scala.collection.immutable.ListMap
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.GridPane

object AnimalProperties {
  val title = "Animal Pane"
  val headerText = "Create an animal"
}

import AnimalProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

case class AnimalPane(mainDialog: MainDialog,
                      override val previousContent: Option[ConfigurationPane],
                      modality: Modality,
                      animal: Option[String] = None)
  extends BackPane[String](mainDialog, previousContent, animal, title, headerText, previousContent.get.path + newLine(1) + title) {

  /*
  Fields
   */

  val typologySet: ObservableBuffer[String] = ObservableBuffer[String]("Carnivorous", "Herbivore")

  val name = new TextField()
  val geneLength = new TextField()
  val alleleLength = new TextField()
  val typology = new ComboBox(typologySet)
  typology.selectionModel().select(0)

  fields = ListMap(
    name -> (new WhiteLabel("Name"), new ErrorLabel("")),
    geneLength -> (new WhiteLabel("Gene Length"), new ErrorLabel("")),
    alleleLength -> (new WhiteLabel("Allele Length"), new ErrorLabel("")),
  )

  val grid: GridPane = createGrid(0)

  grid.add(new WhiteLabel("Typology"), 0, fields.size * 2)
  grid.add(typology, 1, fields.size * 2)

  center = grid

  Platform.runLater(name.requestFocus())

  /*
  Checks
   */

  mandatoryFields = fields.keySet
  intFields = Set(geneLength, alleleLength)
  uniqueFields = Map(name -> (EntitiesInfo.instance().getPlants ++ EntitiesInfo.instance().getAnimals))

  createChecks()

  /*
  Restart information
   */

  if (animal.isDefined) {
    val animalBaseInfo = EntitiesInfo.instance().getAnimalBaseInfo(animal.get)
    name.editable = false
    name.text.value = animal.get
    geneLength.text.value = animalBaseInfo.geneLength.toString
    alleleLength.text.value = animalBaseInfo.alleleLength.toString
    typology.selectionModel().select(animalBaseInfo.typology.toString)
  }

  /*
  Result
   */

  okButton.onAction = _ => {
    EntitiesInfo.instance().setAnimalBaseInfo(name.text.value, AnimalBaseInfo(geneLength.text.value.toInt,
      alleleLength.text.value.toInt, typology.value.value))
    mainDialog.setContent(ChromosomePane(mainDialog, Some(this), AddModality, if (animal.isEmpty) name.text.value else animal.get))
  }

  def confirmChromosome(name: String): Unit = {
    previousContent.get.confirmAnimalSpecies(modality, name)
  }
}

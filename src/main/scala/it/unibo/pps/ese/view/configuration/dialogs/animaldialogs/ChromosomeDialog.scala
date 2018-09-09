package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs


import it.unibo.pps.ese.controller.loader._
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.{CustomGeneDialog, DefaultGeneDialog}
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals.AnimalChromosomeInfo
import it.unibo.pps.ese.view.configuration.entitiesinfo.{ChromosomeTypes, EntitiesInfo}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.Window

case class ChromosomeDialog(window: Window, animal: String) extends AbstractDialog(window, None) {

  /*
  Header
   */

  title = "Chromosome Dialog"
  headerText = "Define animal chromosome"

  /*
  Fields
   */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)

  val structuralName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.structuralChromosome.keySet toSeq)
  val structuralChromosomeListView: ListView[String] = new ListView[String] {
    items = structuralName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        CustomGeneDialog(window, animal, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val regulationName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.regulationChromosome.keySet toSeq)
  val regulationChromosomeListView: ListView[String] = new ListView[String] {
    items = regulationName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        DefaultGeneDialog(window, ChromosomeTypes.REGULATION, animal, Some(value), RegulationDefaultGenes.elements -- getCurrentRegulationChromosome).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val sexualName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.sexualChromosome.keySet toSeq)
  val sexualChromosomeListView: ListView[String] = new ListView[String] {
    items = sexualName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        DefaultGeneDialog(window, ChromosomeTypes.SEXUAL, animal, Some(value), SexualDefaultGenes.elements -- getCurrentSexualChromosome).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  structuralChromosomeListView.prefHeight = MIN_ELEM * ROW_HEIGHT
  regulationChromosomeListView.prefHeight =  MIN_ELEM *  ROW_HEIGHT
  sexualChromosomeListView.prefHeight =  MIN_ELEM *  ROW_HEIGHT

  val structuralButton = new Button("Add")
  structuralButton.onAction = _ => CustomGeneDialog(window, animal, None).showAndWait() match {
    case Some(name) =>
      structuralName.insert(structuralName.size, name.toString)
    case None => println("Dialog returned: None")
  }
  val regulationButton = new Button("Add")
  regulationButton.onAction = _ => {
    DefaultGeneDialog(window, ChromosomeTypes.REGULATION, animal, None, RegulationDefaultGenes.elements -- getCurrentRegulationChromosome).showAndWait() match {
      case Some(name) =>
        regulationName.insert(regulationName.size, name.toString)
        regulationButton.disable = (RegulationDefaultGenes.elements -- getCurrentRegulationChromosome).isEmpty
      case None => println("Dialog returned: None")
    }
  }
  val sexualButton = new Button("Add")
  sexualButton.onAction = _ => {
    DefaultGeneDialog(window, ChromosomeTypes.SEXUAL, animal, None, SexualDefaultGenes.elements -- getCurrentSexualChromosome).showAndWait() match {
      case Some(name) =>
        sexualName.insert(sexualName.size, name.toString)
        sexualButton.disable = (SexualDefaultGenes.elements -- getCurrentSexualChromosome).isEmpty
      case None => println("Dialog returned: None")
    }
  }

  val structuralPane = new BorderPane()
  structuralPane.left = new Label("Structural Chromosome")
  structuralPane.right = structuralButton


  val regulationPane = new BorderPane()
  regulationPane.left = new Label("Regulation Chromosome")
  regulationPane.right = regulationButton

  val sexualPane = new BorderPane()
  sexualPane.left = new Label("Sexual Chromosome")
  sexualPane.right = sexualButton

  dialogPane().content = new VBox() {
    children ++= Seq(structuralPane, structuralChromosomeListView, regulationPane, regulationChromosomeListView,
      sexualPane, sexualChromosomeListView, new Label("At least one element per chromosome"))
    styleClass += "sample-page"
  }

  /*
  Checks
   */

  listFields = Seq(structuralName, regulationName, sexualName)

  createChecks()

  /*
  Support Methods
  */

  private def getCurrentRegulationChromosome: Set[RegulationDefaultGene] =
    currentAnimalChromosome.regulationChromosome.keySet.map(x => RegulationDefaultGenes.elements.filter(y => y.name.equals(x)).head)

  private def getCurrentSexualChromosome: Set[SexualDefaultGene] =
    currentAnimalChromosome.sexualChromosome.keySet.map(x => SexualDefaultGenes.elements.filter(y => y.name.equals(x)).head)

}

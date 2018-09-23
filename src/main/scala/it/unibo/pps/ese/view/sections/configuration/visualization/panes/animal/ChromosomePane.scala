package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal


import it.unibo.pps.ese.controller.simulation.loader._
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.{CustomGenePane, DefaultGenePane}
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{CustomListView, WhiteLabel}
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals.AnimalChromosomeInfo
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.{EntitiesInfo, RegulationChromosome, SexualChromosome}
import it.unibo.pps.ese.view.sections.configuration.visualization.core._

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}

object ChromosomeProperties {
  val title = "Chromosome Pane"
  val headerText = "Define animal chromosome"
}

import ChromosomeProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

case class ChromosomePane(mainDialog: MainDialog,
                          override val previousContent: Option[AnimalPane],
                          modality: Modality,
                          animal: String)
  extends BackPane(mainDialog, previousContent, None, title, headerText, previousContent.get.path + newLine(2) + title) {


  /*
  Fields
   */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)

  val structuralName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.structuralChromosome.keySet toSeq)
  val structuralChromosomeListView: ListView[String] = new CustomListView[String] {
    items = structuralName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(CustomGenePane(mainDialog, Some(ChromosomePane.this), ModifyModality, animal, Some(value)))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val regulationName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.regulationChromosome.keySet toSeq)
  val regulationChromosomeListView: ListView[String] = new CustomListView[String] {
    items = regulationName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(DefaultGenePane(mainDialog, Some(ChromosomePane.this), ModifyModality, RegulationChromosome, animal, Some(value),
          RegulationDefaultGenes.elements -- getCurrentRegulationChromosome))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val sexualName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.sexualChromosome.keySet toSeq)
  val sexualChromosomeListView: ListView[String] = new CustomListView[String] {
    items = sexualName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(DefaultGenePane(mainDialog, Some(ChromosomePane.this), ModifyModality, SexualChromosome, animal, Some(value),
          SexualDefaultGenes.elements -- getCurrentSexualChromosome))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val structuralButton = new Button("Add")
  structuralButton.onAction = _ => mainDialog.setContent(CustomGenePane(mainDialog, Some(ChromosomePane.this), AddModality, animal, None))

  val regulationButton = new Button("Add")
  regulationButton.onAction = _ => {
    mainDialog.setContent(DefaultGenePane(mainDialog, Some(ChromosomePane.this), AddModality, RegulationChromosome, animal, None,
      RegulationDefaultGenes.elements -- getCurrentRegulationChromosome))
  }

  val sexualButton = new Button("Add")
  sexualButton.onAction = _ => {
    mainDialog.setContent(DefaultGenePane(mainDialog, Some(this), AddModality, SexualChromosome, animal, None,
      SexualDefaultGenes.elements -- getCurrentSexualChromosome))
  }

  val structuralPane = new BorderPane()
  structuralPane.left = new WhiteLabel("Structural Chromosome")
  structuralPane.right = structuralButton
  structuralPane.margin = Insets(30,0,0,0)


  val regulationPane = new BorderPane()
  regulationPane.left = new WhiteLabel("Regulation Chromosome")
  regulationPane.right = regulationButton
  regulationPane.margin = Insets(30,0,0,0)

  val sexualPane = new BorderPane()
  sexualPane.left = new WhiteLabel("Sexual Chromosome")
  sexualPane.right = sexualButton
  sexualPane.margin = Insets(30,0,0,0)

  center = new VBox() {
    children ++= Seq(structuralPane, structuralChromosomeListView, regulationPane, regulationChromosomeListView,
      sexualPane, sexualChromosomeListView, new WhiteLabel("At least one element per chromosome"))
    styleClass += "sample-page"
  }

  /*
  Checks
   */

  listFields = Seq((structuralName, 1),
    (regulationName, RegulationDefaultGenes.elements.size),
    (sexualName, SexualDefaultGenes.elements.size))

  createChecks()

  /*
  Support Methods
  */

  private def getCurrentRegulationChromosome: Set[RegulationDefaultGene] =
    currentAnimalChromosome.regulationChromosome.keySet.map(x => RegulationDefaultGenes.elements.filter(y => y.name.equals(x)).head)

  private def getCurrentSexualChromosome: Set[SexualDefaultGene] =
    currentAnimalChromosome.sexualChromosome.keySet.map(x => SexualDefaultGenes.elements.filter(y => y.name.equals(x)).head)

  okButton.onAction = _ => {
    previousContent.get.confirmChromosome(animal)
  }


  def confirmStructuralChromosome(m: Modality, name: String): Unit = {
    m match {
      case AddModality =>
        structuralName.insert(structuralName.size, name)
      case ModifyModality =>
        Platform.runLater(structuralChromosomeListView.selectionModel().clearSelection())
    }
    mainDialog.setContent(this)
  }

  def confirmRegulationChromosome(m: Modality, name: String): Unit = {
    m match {
      case AddModality =>
        regulationName.insert(regulationName.size, name.toString)
        regulationButton.disable = (RegulationDefaultGenes.elements -- getCurrentRegulationChromosome).isEmpty
      case ModifyModality =>
        Platform.runLater(regulationChromosomeListView.selectionModel().clearSelection())
    }
    mainDialog.setContent(this)
  }

  def confirmSexualChromosome(m: Modality, name: String): Unit = {
    m match {
      case AddModality =>
        sexualName.insert(sexualName.size, name.toString)
        sexualButton.disable = (SexualDefaultGenes.elements -- getCurrentSexualChromosome).isEmpty
      case ModifyModality =>
        Platform.runLater(regulationChromosomeListView.selectionModel().clearSelection())

    }
    mainDialog.setContent(this)
  }


}

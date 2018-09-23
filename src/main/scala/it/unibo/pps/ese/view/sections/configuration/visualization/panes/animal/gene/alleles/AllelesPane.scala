package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.alleles

import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.GenePane
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.WhiteLabel
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo._
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals.{AlleleInfo, AnimalChromosomeInfo, ChromosomeInfo}
import it.unibo.pps.ese.view.sections.configuration.visualization.core.{BackPane, MainDialog}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}

object AllelesProperties {
    val title = "Alleles Pane"
    val headerText = "Define chromosome alleles"
}

import AllelesProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

case class AllelesPane(mainDialog: MainDialog,
                       override val previousContent: Option[GenePane],
                       animal: String,
                       gene: String,
                       chromosomeTypes: ChromosomeTypes)
  extends BackPane(mainDialog, previousContent, None, title, headerText, previousContent.get.path + newLine(4) + title) {

  /*
  Fields
  */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)

  val currentSpecificAnimalChromosome: Map[String, ChromosomeInfo] = chromosomeTypes match {
      case StructuralChromosome => currentAnimalChromosome.structuralChromosome
      case RegulationChromosome => currentAnimalChromosome.regulationChromosome
      case SexualChromosome => currentAnimalChromosome.sexualChromosome
    }

  var currentAlleles: Map[String, AlleleInfo] = currentSpecificAnimalChromosome.get(gene) match {
    case Some(chromosomeInfo) => chromosomeInfo.alleles
    case None => throw new IllegalStateException()
  }

  var properties: Set[String] = currentSpecificAnimalChromosome(gene).geneInfo.properties.keySet
  val allelesName: ObservableBuffer[String] = ObservableBuffer[String](currentAlleles.keySet toSeq)
  val allelesListView: ListView[String] = new ListView[String] {
    items = allelesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        val missedProperties: Map[String, Double] = (properties -- currentAlleles(value).effect.keySet).map(x => (x, 0.0)).toMap
        val currentAllele: AlleleInfo = currentAlleles(value)
        currentAllele.effect ++= missedProperties
        currentAlleles += (value -> currentAllele)
        EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
        mainDialog.setContent(AllelePane(mainDialog, Some(AllelesPane.this), animal, gene, Some(value), properties, chromosomeTypes))

        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val allelesButton = new Button("Add")
  allelesButton.onAction = _ => mainDialog.setContent(AllelePane(mainDialog, Some(this), animal, gene, None, properties, chromosomeTypes))

  val allelesPane = new BorderPane()
  allelesPane.left = new WhiteLabel("Alleles")
  allelesPane.right = allelesButton
  allelesPane.margin = Insets(30,0,0,0)

  center = new VBox() {
    children ++= Seq(allelesPane, allelesListView, new Label("At least one allele"))
    styleClass += "sample-page"
  }

  /*
  Checks
   */

  listFields = Seq(allelesName)
  createChecks()

  def confirmAddAlleleInfo(a: AlleleInfo): Unit = {
    currentAlleles += (a.id -> a)
    allelesName.insert(allelesName.size, a.id)
    EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
    mainDialog.setContent(this)
  }

  def confirmModifyAlleleInfo(a: AlleleInfo): Unit = {
    currentAlleles += (a.id -> a)
    EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
    mainDialog.setContent(this)
  }

  okButton.onAction = _ => {
    mainDialog.setContent(previousContent.get)
    previousContent.get.confirmAlleles(gene)
  }

}


package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs

import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.ChromosomePane
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.GenePane
import it.unibo.pps.ese.view.configuration.dialogs.components.WhiteLabel
import it.unibo.pps.ese.view.configuration.entitiesinfo._
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals.{AlleleInfo, AnimalChromosomeInfo, ChromosomeInfo, GeneInfo}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, Pane, VBox}
import scalafx.stage.Window

case class AllelesPane(mainDialog: MainDialog,
                       override val previousContent: Option[GenePane],
                       animal: String,
                       gene: String,
                       chromosomeTypes: ChromosomeTypes.Value)
  extends BackPane(mainDialog, previousContent, None, "", "", "") {

  /*
  Header
  */

//  title = "Alleles Dialog"
//  headerText = "Define chromosome alleles"

  /*
  Fields
  */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)

  val currentSpecificAnimalChromosome: Map[String, ChromosomeInfo] = chromosomeTypes match {
      case ChromosomeTypes.STRUCTURAL => currentAnimalChromosome.structuralChromosome
      case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome
      case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome
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
        val missedProperties: Map[String, Double] = (properties -- currentAlleles(value).effect.keySet).map(x => (x, 0.0)).groupBy(_._1).map{ case (k,v) => (k,v.map(_._2))}.map(x => x._1 -> x._2.head)
        val currentAllele: AlleleInfo = currentAlleles(value)
        currentAllele.effect ++= missedProperties
        currentAlleles += (value -> currentAllele)
        EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
        mainDialog.setContent(AllelePane(mainDialog, Some(AllelesPane.this), animal, gene, Some(value), properties, chromosomeTypes))

        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }



//  allelesListView.prefHeight = MIN_ELEM * ROW_HEIGHT

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


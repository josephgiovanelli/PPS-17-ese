package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.alleles


import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{CustomListView, ErrorLabel, WhiteLabel}
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo._
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals.{AlleleInfo, AnimalChromosomeInfo, ChromosomeInfo}
import it.unibo.pps.ese.view.sections.configuration.visualization.core.{AbstractPane, MainDialog}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, VBox}

/**
  * It defines the title and the header
  */
object AlleleProperties {
    val title = "Allele Dialog"
    val headerText = "Create an allele"
}

import AlleleProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

/**
  * The pane that allows to insert the allele information.
  *
  * @param mainDialog the main dialog with which communicating
  * @param previousContent the previous content
  * @param animal the animal identifier
  * @param gene the gene identifier
  * @param allele the previous allele identifier if is present before
  * @param properties the properties to set
  * @param chromosomeTypes the chromosome type of the gene of the allele
  */
case class AllelePane(mainDialog: MainDialog,
                      override val previousContent: Option[AllelesPane],
                      animal: String,
                      gene: String,
                      allele: Option[String],
                      properties: Set[String],
                      chromosomeTypes: ChromosomeTypes)
  extends AbstractPane[AlleleInfo](mainDialog, previousContent, allele, title, headerText, previousContent.get.path + newLine(5) + title, 5) {

  /*
  Fields
  */


  val idAllele: TextField = new TextField()
  val dominance: TextField = new TextField()
  val consume: TextField = new TextField()
  val probability: TextField = new TextField()

  fields = ListMap(
    idAllele -> (new WhiteLabel("Id"), new ErrorLabel("")),
    dominance -> (new WhiteLabel("Dominance"), new ErrorLabel("")),
    consume -> (new WhiteLabel("Consume"), new ErrorLabel("")),
    probability -> (new WhiteLabel("Probability"), new ErrorLabel("")),
  )


  val grid: GridPane = createGrid(0)

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

  var effects:  Map[String, Double] =
    if (allele.isDefined) currentAlleles(allele.get).effect
    else properties.map(x => (x, 0.0)).toMap
  val effectsName: ObservableBuffer[String] = ObservableBuffer[String](effects.keySet toSeq)
  val effectsListView: ListView[String] = new CustomListView[String] {
    items = effectsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(EffectPane(mainDialog, Some(AllelePane.this), (value, effects(value))))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }


  center = new VBox() {
    children ++= Seq(grid, new WhiteLabel("Effects"), effectsListView, new WhiteLabel("At least one effect"))
    styleClass += "sample-page"
  }

  Platform.runLater(idAllele.requestFocus())

  /*
  Checks
  */

  val allelesId: Set[String] = (currentAnimalChromosome.structuralChromosome ++
    currentAnimalChromosome.regulationChromosome ++
    currentAnimalChromosome.sexualChromosome).values.flatMap(x => x.alleles.keySet) toSet

  mandatoryFields = fields.keySet
  doubleFields = mandatoryFields - idAllele
  listFields = Seq((effectsName, 1))
  uniqueFields = Map(idAllele -> allelesId)
  lengthFields = Map(idAllele -> EntitiesInfo.instance().getAnimalBaseInfo(animal).alleleLength)
  probabilityFields = Set(probability)

  createChecks()


  /*
  Restart information
  */

  if (allele.isDefined) {
    idAllele.editable = false
    idAllele.text.value = currentAlleles(allele.get).id
    dominance.text.value = currentAlleles(allele.get).dominance.toString
    consume.text.value = currentAlleles(allele.get).consume.toString
    probability.text.value = currentAlleles(allele.get).probability.toString
  }

  /*
  Result
   */

  okButton.onAction = _ => {
    allele match {
      case Some(_) => previousContent.get.confirmModifyAlleleInfo(AlleleInfo(gene, idAllele.text.value, dominance.text.value.toDouble,
        consume.text.value.toDouble, probability.text.value.toDouble, effects))
      case None => previousContent.get.confirmAddAlleleInfo(AlleleInfo(gene, idAllele.text.value, dominance.text.value.toDouble,
        consume.text.value.toDouble, probability.text.value.toDouble, effects))
    }
  }

  def confirmAddEffect(name: String, value: Double): Unit = {
    effects += (name -> value)
    mainDialog.setContent(this)
  }
}


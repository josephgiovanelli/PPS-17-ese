package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs

import it.unibo.pps.ese.controller.loader._
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.ChromosomePane
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs.AllelesPane
import it.unibo.pps.ese.view.configuration.entitiesinfo._
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals.{AlleleInfo, AnimalChromosomeInfo, DefaultChromosomeInfo, DefaultGeneInfo}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, Pane}
import scalafx.stage.Window

abstract class GenePane(mainDialog: MainDialog,
                        override val previousContent: Option[ChromosomePane],
                        gene: Option[String]) extends BackPane[String](mainDialog, previousContent, gene) {


  def confirmAddAlleles(defaultGene: DefaultGene)
}

case class DefaultGenePane(mainDialog: MainDialog,
                           override val previousContent: Option[ChromosomePane],
                           chromosomeTypes: ChromosomeTypes.Value,
                           animal: String,
                           gene: Option[String],
                           propertiesSet: Set[_ <: DefaultGene]) extends GenePane(mainDialog, previousContent, gene) {

  /*
  Header
   */

  mainDialog.title = "Default Gene Dialog"
  mainDialog.headerText = "Define " + chromosomeTypes.toString.toLowerCase + " chromosome"

  /*
  Fields
   */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)


  val currentDefaultChromosome: Map[String, DefaultChromosomeInfo] = chromosomeTypes match {
    case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome
    case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome
  }

  val propertiesName: ObservableBuffer[String] = ObservableBuffer[String](propertiesSet.map(x => x.name) toSeq)

  val idGene: TextField = new TextField()

  val nameGene = new ComboBox(propertiesName)
  val previousNameGene = new TextField()

  fields = ListMap(
    idGene -> (new Label("Id"), new Label(""))
  )

  val grid: GridPane = createGrid(0)

  grid.add(new Label("Name"), 0, fields.size * 2)
  grid.add(if (gene.isDefined) previousNameGene else nameGene, 1, fields.size * 2)


  mainDialog.setContent(grid)


  /*
  Checks
   */

  val genes: Set[String] = (currentAnimalChromosome.structuralChromosome ++
    currentAnimalChromosome.regulationChromosome ++
    currentAnimalChromosome.sexualChromosome).values.map(x => x.geneInfo.id) toSet

  mandatoryFields = fields.keySet
  uniqueFields = Map(idGene -> genes)
  lengthFields = Map(idGene -> EntitiesInfo.instance().getAnimalBaseInfo(animal).geneLength)


  createChecks()

  /*
  Restart information
  */

  if (gene.isDefined) {
    idGene.editable = false
    idGene.text.value = currentDefaultChromosome(gene.get).geneInfo.id
    previousNameGene.editable = false
    previousNameGene.text.value = currentDefaultChromosome(gene.get).geneInfo.properties.head._1
    nameGene.selectionModel().select(currentDefaultChromosome(gene.get).geneInfo.properties.head._1)
  } else {
    nameGene.value.value = propertiesName.head
  }

  /*
  Result
  */

  okButton.onAction = _ => {
    val defaultGene: DefaultGene = if (gene.isDefined) currentDefaultChromosome(gene.get).geneInfo.defaultGene
    else propertiesSet.filter(x => x.name.equals(nameGene.selectionModel().getSelectedItem)).head
    EntitiesInfo.instance().setChromosomeBaseInfo(animal, chromosomeTypes, DefaultGeneInfo(defaultGene, idGene.text.value))
    mainDialog.setContent(AllelesPane(mainDialog, Some(this), animal, defaultGene.name, chromosomeTypes))
  }

  override def confirmAddAlleles(defaultGene: DefaultGene): Unit = {
    chromosomeTypes match {
      case ChromosomeTypes.REGULATION =>
        previousContent.get.confirmAddRegulationChromosome(defaultGene.name)
        mainDialog.setContent(this)
      case ChromosomeTypes.SEXUAL =>
        previousContent.get.confirmAddSexualChromosome(defaultGene.name)
        mainDialog.setContent(this)
    }
  }

}



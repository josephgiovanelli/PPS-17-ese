package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene

import it.unibo.pps.ese.controller.simulation.loader._
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.ChromosomePane
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.alleles.AllelesPane
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.WhiteLabel
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo._
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals.{AnimalChromosomeInfo, DefaultChromosomeInfo, DefaultGeneInfo}
import it.unibo.pps.ese.view.sections.configuration.visualization.core.{BackPane, MainDialog, Modality}

import scala.collection.immutable.ListMap
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.GridPane

abstract class GenePane(mainDialog: MainDialog,
                        override val previousContent: Option[ChromosomePane],
                        gene: Option[String],
                        title: String,
                        headerText: String,
                        path: String)
  extends BackPane[String](mainDialog, previousContent, gene, title, headerText, path) {


  def confirmAlleles(gene: String)
}

object DefaultGeneProperties {
    val title = "Default Gene Pane"
    val headerText = "Define your chromosome"
}

import DefaultGeneProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

case class DefaultGenePane(mainDialog: MainDialog,
                           override val previousContent: Option[ChromosomePane],
                           modality: Modality,
                           chromosomeTypes: ChromosomeTypes,
                           animal: String,
                           gene: Option[String],
                           propertiesSet: Set[_ <: DefaultGene])
  extends GenePane(mainDialog, previousContent, gene, title, headerText, previousContent.get.path + newLine(3)) {

  /*
  Fields
   */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)


  val currentDefaultChromosome: Map[String, DefaultChromosomeInfo] = chromosomeTypes match {
    case RegulationChromosome => currentAnimalChromosome.regulationChromosome
    case SexualChromosome => currentAnimalChromosome.sexualChromosome
  }

  val propertiesName: ObservableBuffer[String] = ObservableBuffer[String](propertiesSet.map(x => x.name) toSeq)

  val idGene: TextField = new TextField()

  val nameGene = new ComboBox(propertiesName)
  val previousNameGene = new TextField()

  fields = ListMap(
    idGene -> (new WhiteLabel("Id"), new WhiteLabel(""))
  )

  val grid: GridPane = createGrid(0)

  grid.add(new WhiteLabel("Name"), 0, fields.size * 2)
  grid.add(if (gene.isDefined) previousNameGene else nameGene, 1, fields.size * 2)


  center = grid


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

  override def confirmAlleles(gene: String): Unit = {
    chromosomeTypes match {
      case RegulationChromosome=>
        previousContent.get.confirmRegulationChromosome(modality, gene)
      case SexualChromosome =>
        previousContent.get.confirmSexualChromosome(modality, gene)
    }
  }

}



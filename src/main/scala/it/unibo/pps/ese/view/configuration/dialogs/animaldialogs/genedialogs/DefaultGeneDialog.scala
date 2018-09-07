package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs

import javafx.scene.Node

import it.unibo.pps.ese.controller.loader._
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs.AllelesDialog

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class DefaultGeneDialog(window: Window, chromosomeTypes: ChromosomeTypes.Value, animal: String, gene: Option[String], propertiesSet: Set[_ <: DefaultGene] ) extends AbstractDialog[String](window, gene) {

  /*
  Header
   */

  title = "Default Gene Dialog"
  headerText = "Define " + chromosomeTypes.toString.toLowerCase + " chromosome"

  /*
  Fields
   */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo
    case None => throw new IllegalStateException()
  }

  val currentDefaultChromosome: Map[String, (DefaultGeneInfo, Map[String, AlleleInfo])] = chromosomeTypes match {
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

  val grid: GridPane = createGrid

  grid.add(new Label("Name"), 0, fields.size * 2)
  grid.add(if (gene.isDefined) previousNameGene else nameGene, 1, fields.size * 2)


  dialogPane().content = grid


  /*
  Checks
   */

  val genes = (currentAnimalChromosome.structuralChromosome ++
    currentAnimalChromosome.regulationChromosome ++
    currentAnimalChromosome.sexualChromosome).values.map(x => x._1.id) toSet

  mandatoryFields = fields.keySet
  uniqueFields = Map(idGene -> genes)
  lengthFields = Map(idGene -> EntitiesInfo.instance().getAnimalInfo(animal).get._1.geneLength)


  createChecks()

  /*
  Restart information
  */

  if (gene.isDefined) {
    idGene.editable = false
    idGene.text.value = currentDefaultChromosome(gene.get)._1.id
    previousNameGene.editable = false
    previousNameGene.text.value = currentDefaultChromosome(gene.get)._1.properties.head._1
    nameGene.selectionModel().select(currentDefaultChromosome(gene.get)._1.properties.head._1)
  } else {
    nameGene.value.value = propertiesName.head
  }

  /*
  Result
  */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      val defaultGene: DefaultGene = if (gene.isDefined) currentDefaultChromosome(gene.get)._1.defaultGene
                                     else propertiesSet.filter(x => x.name.equals(nameGene.selectionModel().getSelectedItem)).head
      EntitiesInfo.instance().setChromosomeBaseInfo(animal, chromosomeTypes, DefaultGeneInfo(defaultGene, idGene.text.value))
      AllelesDialog(window, animal, defaultGene.name, chromosomeTypes).showAndWait()
      defaultGene.name
    } else {
      null
    }

}



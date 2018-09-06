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

case class DefaultGeneDialog(window: Window, chromosomeTypes: ChromosomeTypes.Value, animal: String, gene: Option[String], propertiesSet: Set[_ <: DefaultGene] ) extends Dialog[String] {

  /*
  Header
   */

  initOwner(window)
  title = "Default Gene Dialog"
  headerText = "Define " + chromosomeTypes.toString.toLowerCase + " chromosome"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

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

  val fields: Map[TextField, (Label, Label)] = ListMap(
    idGene -> (new Label("Id"), new Label(""))
  )

  val grid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(10, 100, 10, 10)

    var count = 0
    fields.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
    add(new Label("Name"), 0, count)
    add(if (gene.isDefined) previousNameGene else nameGene, 1, count)
  }

  dialogPane().content = grid

  /*
  OkButton
   */

  val okButtonType = new ButtonType("Insert Alleles", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  /*
  Checks
   */

  val mandatoryFields: Set[TextField] = fields.keySet
  val genes: Map[String, (GeneInfo, Map[String, AlleleInfo])] = currentAnimalChromosome.structuralChromosome ++
    currentAnimalChromosome.regulationChromosome ++
    currentAnimalChromosome.sexualChromosome

  mandatoryFields.foreach(subject =>
    subject.text.onChange ((_, _, newValue) =>
      okButton.disable = checkFields(subject, newValue)))

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

  private def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatoryCheck = field.getText.trim().isEmpty
    val lengthCheck = if (field.equals(idGene)) idGene.text.value.length != EntitiesInfo.instance().getAnimalInfo(animal).get._1.geneLength
                      else false
    val uniqueIdCheck = if (field.equals(idGene) && gene.isEmpty) genes.values.map(x => x._1.id).toSet.contains(idGene.text.value)
                        else false

    if (mandatoryCheck || lengthCheck || uniqueIdCheck)
      field.pseudoClassStateChanged(errorClass, true)
    else
      field.pseudoClassStateChanged(errorClass, false)

    if (mandatoryCheck) fields(field)._2.text.value = "Must be filled"
    else if (lengthCheck) fields(field)._2.text.value = "Must be " + EntitiesInfo.instance().getAnimalInfo(animal).get._1.geneLength + " long"
    else if (uniqueIdCheck) fields(field)._2.text.value = "Must be unique"
    else fields(field)._2.text.value = ""
    checkFields
  }

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) ||
    idGene.text.value.length != EntitiesInfo.instance().getAnimalInfo(animal).get._1.geneLength ||
    (genes.values.map(x => x._1.id).toSet.contains(idGene.text.value) && gene.isEmpty)


}



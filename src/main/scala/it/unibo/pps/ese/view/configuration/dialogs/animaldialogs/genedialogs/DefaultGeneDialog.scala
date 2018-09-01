package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs

import javafx.scene.Node

import it.unibo.pps.ese.controller.loader._
import it.unibo.pps.ese.view.configuration.dialogs.ConversionMap
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs.AllelesDialog
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog.PropertiesDialog

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.stage.Window

case class DefaultGeneDialog(window: Window, chromosomeTypes: ChromosomeTypes.Value, animal: String, gene: Option[String] = None) extends Dialog[String] {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)
  title = "Default Gene Dialog"
  headerText = "Define " + chromosomeTypes.toString.toLowerCase + " chromosome"

  // Set the button types.
  val okButtonType = new ButtonType("Insert Alleles", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo
    case None => throw new IllegalStateException()
  }

  var currentDefaultChromosome: Map[String, (DefaultGeneInfo, Map[String, AlleleInfo])] = chromosomeTypes match {
    case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome
    case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome
  }

  val propertiesSet = chromosomeTypes match {
    case ChromosomeTypes.REGULATION => RegulationDefaultGenes.elements
    case ChromosomeTypes.SEXUAL => SexualDefaultGenes.elements
  }

  val propertiesName: ObservableBuffer[String] = ObservableBuffer[String]((propertiesSet.map(x => x.name) -- currentDefaultChromosome.keySet) toSeq)

  val idGene: TextField = new TextField() {
    promptText = "Id"
  }
  val nameGene = new ComboBox(propertiesName)

  val previousNameGene = new TextField()

  val requiredField = Seq(idGene, if (gene.isDefined) previousNameGene else nameGene)

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Id"), 0, 0)
    add(idGene, 1, 0)
    add(new Label("Name"), 0, 1)
    add(if (gene.isDefined) previousNameGene else nameGene, 1, 1)
  }


  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(grid)
    styleClass += "sample-page"
  }

  if (gene.isDefined) {
    idGene.editable = false
    idGene.text.value = currentDefaultChromosome(gene.get)._1.id
    previousNameGene.editable = false
    previousNameGene.text.value = currentDefaultChromosome(gene.get)._1.properties.head._1
    nameGene.selectionModel().select(currentDefaultChromosome(gene.get)._1.properties.head._1)
  }

  // When the login button is clicked, convert the result to
  // a username-password-pair.

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      val defaultGene = propertiesSet.filter(x => x.name.equals(nameGene.selectionModel().getSelectedItem)).head
      println(chromosomeTypes)
      EntitiesInfo.instance().setChromosomeBaseInfo(animal, chromosomeTypes, DefaultGeneInfo(defaultGene, idGene.text.value))
      println(EntitiesInfo.instance().getAnimalInfo(animal).get._2)
      AllelesDialog(window, animal, defaultGene.name, chromosomeTypes).showAndWait()
      defaultGene.name
    } else {
      null
    }

}



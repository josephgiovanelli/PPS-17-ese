package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs


import javafx.scene.Node

import it.unibo.pps.ese.controller.loader.data.AlleleData
import it.unibo.pps.ese.view.configuration.dialogs.plantdialogs.PlantDialog
import it.unibo.pps.ese.view.configuration.dialogs._

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.stage.Window

case class CustomGeneDialog(window: Window, animal: String, gene: Option[String] = None) extends Dialog[String] {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)
  title = "Custom Gene Dialog"
  headerText = "Define structural chromosome"

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo
    case None => throw new IllegalStateException()
  }

  var currentStructuralChromosome: Map[String, CustomGeneInfo] = currentAnimalChromosome.structuralChromosome

  if (gene.isDefined) println((animal, gene.get))

  val idGene: TextField = new TextField() {
    promptText = "Id"
  }
  val nameGene: TextField = new TextField() {
    promptText = "Name"
  }

  val requiredField = Seq(idGene, nameGene)

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Id"), 0, 0)
    add(idGene, 1, 0)
    add(new Label("Name"), 0, 1)
    add(nameGene, 1, 1)
  }

  val properties: Map[String, Class[_]] = if (gene.isDefined) currentStructuralChromosome(gene.get).properties else Map.empty
  val propertiesName: ObservableBuffer[String] = ObservableBuffer[String](properties.keySet toSeq)
  val propertiesListView: ListView[String] = new ListView[String] {
    items = propertiesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        PlantDialog(window, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  var alleles: Set[AlleleData] = if (gene.isDefined) currentStructuralChromosome(gene.get).alleles else Set.empty
  val allelesName: ObservableBuffer[String] = ObservableBuffer[String](alleles map (x => x.id) toSeq)
  val allelesListView: ListView[String] = new ListView[String] {
    items = allelesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        PlantDialog(window, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }


  propertiesListView.prefHeight = MIN_ELEM * ROW_HEIGHT
  allelesListView.prefHeight = MIN_ELEM * ROW_HEIGHT

  val propertiesButton = new Button("Add")
  propertiesButton.onAction = _ => PlantDialog(window).showAndWait() match {
    case Some(name) => {
      propertiesName.insert(propertiesName.size, name.toString)
    }
    case None => println("Dialog returned: None")
  }
  val allelesButton = new Button("Add")
  allelesButton.onAction = _ => PlantDialog(window).showAndWait() match {
    case Some(name) => {
      allelesName.insert(allelesName.size, name.toString)
    }
    case None => println("Dialog returned: None")
  }

  val propertiesPane = new BorderPane()
  propertiesPane.left = new Label("Properties")
  propertiesPane.right = propertiesButton

  val allelesPane = new BorderPane()
  allelesPane.left = new Label("Alleles")
  allelesPane.right = allelesButton


  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(grid, propertiesPane, propertiesListView, allelesPane, allelesListView)
    styleClass += "sample-page"
  }
  if (gene.isDefined) {
    nameGene.editable = false
    nameGene.text.value = currentStructuralChromosome(gene.get).name
    idGene.editable = false
    idGene.text.value = currentStructuralChromosome(gene.get).id
  }

  // When the login button is clicked, convert the result to
  // a username-password-pair.

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      currentStructuralChromosome += (nameGene.text.value -> CustomGeneInfo(idGene.text.value, nameGene.text.value, properties, alleles, Map.empty))
      currentAnimalChromosome.structuralChromosome = currentStructuralChromosome
      EntitiesInfo.instance().setAnimalChromosomeInfo(animal, currentAnimalChromosome)
      nameGene.text.value
    } else {
      null
    }

}


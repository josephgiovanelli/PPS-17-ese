package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs


import javafx.scene.Node

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

case class CustomGeneDialog(window: Window, animal: String, gene: Option[String] = None) extends Dialog[String] {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)
  title = "Custom Gene Dialog"
  headerText = "Define structural chromosome"

  // Set the button types.
  val okButtonType = new ButtonType("Insert Alleles", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo
    case None => throw new IllegalStateException()
  }

  var currentStructuralChromosome: Map[String, (CustomGeneInfo, Map[String, AlleleInfo])] = currentAnimalChromosome.structuralChromosome

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

  var properties: Map[String, Class[_]] = if (gene.isDefined) currentStructuralChromosome(gene.get)._1.properties else Map.empty
  //var alleles: Set[AlleleData] = if (gene.isDefined) currentStructuralChromosome(gene.get).alleles else Set.empty
  var conversionMap: Map[String, Map[String, Double]] = if (gene.isDefined) currentStructuralChromosome(gene.get)._1.conversionMap else Map.empty

  val propertiesName: ObservableBuffer[String] = ObservableBuffer[String](properties.keySet toSeq)
  val propertiesListView: ListView[String] = new ListView[String] {
    items = propertiesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        PropertiesDialog(window, animal, gene,  Some(value), if (conversionMap.isEmpty) None else Some(conversionMap(value))).showAndWait() match {
          case Some(ConversionMap(propertyName, map)) => {
            conversionMap += (propertyName -> map)
          }
          case None => println("Dialog returned: None")
        }
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }


  /*val allelesName: ObservableBuffer[String] = ObservableBuffer[String](alleles map (x => x.id) toSeq)
  val allelesListView: ListView[String] = new ListView[String] {
    items = allelesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        PlantDialog(window, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }*/


  propertiesListView.prefHeight = MIN_ELEM * ROW_HEIGHT
  //allelesListView.prefHeight = MIN_ELEM * ROW_HEIGHT

  val propertiesButton = new Button("Add")
  propertiesButton.onAction = _ => PropertiesDialog(window, animal, None, None, None).showAndWait() match {
    case Some(ConversionMap(propertyName, map)) => {
      conversionMap += (propertyName -> map)
      properties += (propertyName -> Double.getClass)
      propertiesName.insert(propertiesName.size, propertyName)
    }
    case None => println("Dialog returned: None")
  }
  /*val allelesButton = new Button("Add")
  allelesButton.onAction = _ => PlantDialog(window).showAndWait() match {
    case Some(name) => {
      allelesName.insert(allelesName.size, name.toString)
    }
    case None => println("Dialog returned: None")
  }*/

  val propertiesPane = new BorderPane()
  propertiesPane.left = new Label("Properties")
  propertiesPane.right = propertiesButton

  /*val allelesPane = new BorderPane()
  allelesPane.left = new Label("Alleles")
  allelesPane.right = allelesButton*/


  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(grid, propertiesPane, propertiesListView/*, allelesPane, allelesListView*/)
    styleClass += "sample-page"
  }
  if (gene.isDefined) {
    nameGene.editable = false
    nameGene.text.value = currentStructuralChromosome(gene.get)._1.name
    idGene.editable = false
    idGene.text.value = currentStructuralChromosome(gene.get)._1.id
  }

  // When the login button is clicked, convert the result to
  // a username-password-pair.

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      EntitiesInfo.instance().setChromosomeBaseInfo(animal, ChromosomeTypes.STRUCTURAL, CustomGeneInfo(idGene.text.value, nameGene.text.value, properties, conversionMap))
      println(EntitiesInfo.instance().getAnimalInfo(animal).get._2)
      AllelesDialog(window, animal, nameGene.text.value, ChromosomeTypes.STRUCTURAL).showAndWait()
      nameGene.text.value
    } else {
      null
    }

}


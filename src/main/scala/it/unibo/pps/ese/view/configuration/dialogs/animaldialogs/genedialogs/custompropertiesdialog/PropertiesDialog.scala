package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs._

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.stage.Window

case class PropertiesDialog(window: Window, animal: String, gene: Option[String], property: Option[String], currentConversionMap: Option[Map[String, Double]]) extends Dialog[ConversionMap] {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)
  title = "Properties Dialog"
  headerText = "Define gene properties"

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo
    case None => throw new IllegalStateException()
  }

  var currentStructuralChromosome: Map[String, (CustomGeneInfo, Map[String, AlleleInfo])] = currentAnimalChromosome.structuralChromosome

  val propertyName: TextField = new TextField() {
    promptText = "Name"
  }

  val requiredField = Seq(propertyName)

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Name"), 0, 0)
    add(propertyName, 1, 0)
  }

  var conversionMap:  Map[String, Double] =
    if (currentConversionMap.isDefined) currentConversionMap.get
    else if (gene.isDefined && property.isDefined) currentStructuralChromosome(gene.get)._1.conversionMap(property.get)
    else Map.empty
  val conversionMapName: ObservableBuffer[String] = ObservableBuffer[String](conversionMap.keySet toSeq)
  val conversionMapListView: ListView[String] = new ListView[String] {
    items = conversionMapName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        ConversionMapDialog(window, Some((value, conversionMap(value)))).showAndWait() match {
          case Some((name: String, value: Double)) =>
            conversionMap += (name -> value)
          case None => println("Dialog returned: None")
        }
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  conversionMapListView.prefHeight = MIN_ELEM * ROW_HEIGHT

  val conversionMapButton = new Button("Add")
  conversionMapButton.onAction = _ => ConversionMapDialog(window, None).showAndWait() match {
    case Some((name: String, value: Double)) =>
      conversionMap += (name -> value)
      conversionMapName.insert(conversionMapName.size, name)
    case None => println("Dialog returned: None")
  }

  val conversionMapPane = new BorderPane()
  conversionMapPane.left = new Label("Conversion Map")
  conversionMapPane.right = conversionMapButton



  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(grid, conversionMapPane, conversionMapListView)
    styleClass += "sample-page"
  }
  if (property.isDefined) {
    propertyName.editable = false
    propertyName.text.value = property.get
  }

  // When the login button is clicked, convert the result to
  // a username-password-pair.

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) ConversionMap(propertyName.text.value, conversionMap)
    else null



}



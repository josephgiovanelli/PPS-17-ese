package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog

import javafx.scene.Node

import it.unibo.pps.ese.genetics.entities.QualityType
import it.unibo.pps.ese.view.configuration.dialogs._

import scala.collection.immutable.ListMap
import scala.language.postfixOps
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class PropertiesDialog(window: Window, animal: String, gene: Option[String], property: Option[String], currentConversionMap: Option[Map[String, Double]]) extends Dialog[ConversionMap] {

  /*
  Header
   */

  initOwner(window)
  title = "Properties Dialog"
  headerText = "Define gene properties"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  /*
  Fields
  */

  val propertyName: TextField = new TextField()
  val fields: Map[TextField, (Label, Label)] = ListMap(
    propertyName -> (new Label("Name"), new Label("")),
  )

  val grid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(20, 100, 10, 10)

    var count = 0
    fields.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
  }


  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo
    case None => throw new IllegalStateException()
  }

  var currentStructuralChromosome: Map[String, (CustomGeneInfo, Map[String, AlleleInfo])] = currentAnimalChromosome.structuralChromosome

  var conversionMap:  Map[String, Double] =
    if (currentConversionMap.isDefined) currentConversionMap.get
    else if (gene.isDefined && property.isDefined) currentStructuralChromosome(gene.get)._1.conversionMap(property.get)
    else Map.empty

  var qualities: Set[String] = QualityType.values.map(x => x.toString).toSet -- conversionMap.keySet

  val conversionMapName: ObservableBuffer[String] = ObservableBuffer[String](conversionMap.keySet toSeq)
  val conversionMapListView: ListView[String] = new ListView[String] {
    items = conversionMapName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        ConversionMapDialog(window, Some((value, conversionMap(value))), qualities).showAndWait() match {
          case Some((name: String, value: Double)) =>
            conversionMap += (name -> value)
          case None => println("Dialog returned: None")
        }
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  conversionMapListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT

  val conversionMapButton = new Button("Add")
  conversionMapButton.onAction = _ => ConversionMapDialog(window, None, qualities).showAndWait() match {
    case Some((name: String, value: Double)) =>
      conversionMap += (name -> value)
      conversionMapName.insert(conversionMapName.size, name)
      qualities -= name
      conversionMapButton.disable = qualities.isEmpty
    case None => println("Dialog returned: None")
  }

  val conversionMapPane = new BorderPane()
  conversionMapPane.left = new Label("Conversion Map")
  conversionMapPane.right = conversionMapButton

  dialogPane().content = new VBox() {
    children ++= Seq(grid, conversionMapPane, conversionMapListView, new Label("At least one conversion map"))
    styleClass += "sample-page"
  }

  /*
  OkButton
  */

  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  /*
  Checks
   */

  val mandatoryFields: Set[TextField] = fields.keySet

  mandatoryFields.foreach(subject =>
    subject.text.onChange ((_, _, newValue) =>
      okButton.disable = checkFields(subject, newValue)))

  conversionMapName.onChange((_,_) =>
    okButton.disable = checkFields)

  /*
  Restart information
  */

  if (property.isDefined) {
    propertyName.editable = false
    propertyName.text.value = property.get
  }

  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) ConversionMap(propertyName.text.value, conversionMap)
    else null

  private def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatoryCheck = field.getText.trim().isEmpty

    if (mandatoryCheck) {
      field.pseudoClassStateChanged(errorClass, true)
      fields(field)._2.text.value = "Must be filled"
    }
    else {
      field.pseudoClassStateChanged(errorClass, false)
      fields(field)._2.text.value = ""
    }
    checkFields
  }

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) || conversionMapName.isEmpty


}



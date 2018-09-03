package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs


import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.ConversionMap
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs.AllelesDialog
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog.PropertiesDialog

import scala.collection.immutable.ListMap
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

case class CustomGeneDialog(window: Window, animal: String, gene: Option[String] = None) extends Dialog[String] {

  /*
  Header
   */

  initOwner(window)
  title = "Custom Gene Dialog"
  headerText = "Define structural chromosome"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  /*
  Fields
   */

  val idGene: TextField = new TextField()
  val nameGene: TextField = new TextField()

  val fields: Map[TextField, (Label, Label)] = ListMap(
    idGene -> (new Label("Id"), new Label("")),
    nameGene -> (new Label("Name"), new Label(""))
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

  var properties: Map[String, Class[_]] = if (gene.isDefined) currentStructuralChromosome(gene.get)._1.properties else Map.empty
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

  propertiesListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT

  val propertiesButton = new Button("Add")
  propertiesButton.onAction = _ => PropertiesDialog(window, animal, None, None, None).showAndWait() match {
    case Some(ConversionMap(propertyName, map)) => {
      conversionMap += (propertyName -> map)
      properties += (propertyName -> Double.getClass)
      propertiesName.insert(propertiesName.size, propertyName)
    }
    case None => println("Dialog returned: None")
  }


  val propertiesPane = new BorderPane()
  propertiesPane.left = new Label("Properties")
  propertiesPane.right = propertiesButton

  dialogPane().content = new VBox() {
    children ++= Seq(grid, propertiesPane, propertiesListView,  new Label("At least one property"))
    styleClass += "sample-page"
  }


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

  mandatoryFields.foreach(subject =>
    subject.text.onChange ((_, _, newValue) =>
      okButton.disable = checkFields(subject, newValue)))

  propertiesName.onChange((_,_) =>
    okButton.disable = checkFields)

  /*
  Restart information
  */

  if (gene.isDefined) {
    nameGene.editable = false
    nameGene.text.value = currentStructuralChromosome(gene.get)._1.name
    idGene.editable = false
    idGene.text.value = currentStructuralChromosome(gene.get)._1.id
  }


  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      EntitiesInfo.instance().setChromosomeBaseInfo(animal, ChromosomeTypes.STRUCTURAL, CustomGeneInfo(idGene.text.value, nameGene.text.value, properties, conversionMap))
      AllelesDialog(window, animal, nameGene.text.value, ChromosomeTypes.STRUCTURAL).showAndWait()
      nameGene.text.value
    } else {
      null
    }

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

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) || propertiesName.isEmpty

}


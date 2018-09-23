package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.customproperties


import it.unibo.pps.ese.model.genetics.entities.QualityType
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.CustomGenePane
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{CustomListView, ErrorLabel, WhiteLabel}
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo._
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals._
import it.unibo.pps.ese.view.sections.configuration.visualization.core._

import scala.collection.immutable.ListMap
import scala.language.postfixOps
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}

object PropertiesProperties {
  val title = "Properties Pane"
  val headerText = "Define gene properties"
}

import PropertiesProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

case class PropertiesPane(mainDialog: MainDialog,
                          override val previousContent: Option[CustomGenePane],
                          modality: Modality,
                          animal: String,
                          gene: Option[String],
                          property: Option[String],
                          currentConversionMap: Option[Map[String, Double]],
                          properties: Iterable[String])
  extends AbstractPane[ConversionMap](mainDialog, previousContent, property, title, headerText, previousContent.get.path + newLine(4) + title, 4) {

  /*
  Fields
  */

  val propertyName: TextField = new TextField()
  fields = ListMap(
    propertyName -> (new WhiteLabel("Name"), new ErrorLabel("")),
  )

  val grid: GridPane = createGrid(0)


  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)

  var currentStructuralChromosome: Map[String, CustomChromosomeInfo] = currentAnimalChromosome.structuralChromosome

  var conversionMap:  Map[String, Double] =
    if (currentConversionMap.isDefined) currentConversionMap.get
    else if (gene.isDefined && property.isDefined) currentStructuralChromosome(gene.get).geneInfo.conversionMap(property.get)
    else Map.empty

  var qualities: Set[String] = QualityType.values.map(x => x.toString).toSet -- conversionMap.keySet

  val conversionMapName: ObservableBuffer[String] = ObservableBuffer[String](conversionMap.keySet toSeq)
  val conversionMapListView: ListView[String] = new CustomListView[String] {
    items = conversionMapName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(ConversionMapPane(
          mainDialog, Some(PropertiesPane.this), ModifyModality, Some((value, conversionMap(value))), qualities))

        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val conversionMapButton = new Button("Add")
  conversionMapButton.onAction = _ => mainDialog.setContent(ConversionMapPane(
    mainDialog, Some(PropertiesPane.this), AddModality, None, qualities))


  val conversionMapPane = new BorderPane()
  conversionMapPane.left = new WhiteLabel("Conversion Map")
  conversionMapPane.right = conversionMapButton
  conversionMapPane.margin = Insets(30,0,0,0)

  center = new VBox() {
    children ++= Seq(grid, conversionMapPane, conversionMapListView, new WhiteLabel("At least one conversion map"))
    styleClass += "sample-page"
  }

  /*
  Checks
   */

  mandatoryFields = fields.keySet
  listFields = Seq((conversionMapName, 1))
  uniqueFields = Map(propertyName -> properties.toSet)

 createChecks()

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

  okButton.onAction = _ => {
    previousContent.get.confirmProperties(modality, ConversionMap(propertyName.text.value, conversionMap))
  }

  def confirmConversionMap(m: Modality, name: String, value: Double): Unit = {
    m match {
      case AddModality =>
        conversionMap += (name -> value)
        conversionMapName.insert(conversionMapName.size, name)
        qualities -= name
        conversionMapButton.disable = qualities.isEmpty
      case ModifyModality =>
        conversionMap += (name -> value)
    }
    mainDialog.setContent(this)

  }
}



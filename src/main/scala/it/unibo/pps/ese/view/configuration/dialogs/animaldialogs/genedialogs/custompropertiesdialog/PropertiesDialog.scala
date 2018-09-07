package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog


import it.unibo.pps.ese.genetics.entities.QualityType
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.entitiesinfo._
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals._

import scala.collection.immutable.ListMap
import scala.language.postfixOps
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.stage.Window

case class PropertiesDialog(window: Window, animal: String, gene: Option[String], property: Option[String], currentConversionMap: Option[Map[String, Double]], properties: Iterable[String]) extends AbstractDialog[ConversionMap](window, property) {

  /*
  Header
   */

  title = "Properties Dialog"
  headerText = "Define gene properties"

  /*
  Fields
  */

  val propertyName: TextField = new TextField()
  fields = ListMap(
    propertyName -> (new Label("Name"), new Label("")),
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
  Checks
   */

  mandatoryFields = fields.keySet
  listFields = Seq(conversionMapName)
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

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) ConversionMap(propertyName.text.value, conversionMap)
    else null

}



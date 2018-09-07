package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs



import it.unibo.pps.ese.controller.loader.{RegulationDefaultGenes, SexualDefaultGenes}
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs.AllelesDialog
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog.PropertiesDialog
import it.unibo.pps.ese.view.configuration.entitiesinfo._
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals._

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.stage.Window

case class CustomGeneDialog(window: Window, animal: String, gene: Option[String] = None) extends AbstractDialog[String](window, gene) {

  /*
  Header
   */

  title = "Custom Gene Dialog"
  headerText = "Define structural chromosome"

  /*
  Fields
   */

  val idGene: TextField = new TextField()
  val nameGene: TextField = new TextField()

  fields = ListMap(
    idGene -> (new Label("Id"), new Label("")),
    nameGene -> (new Label("Name"), new Label(""))
  )

  val grid: GridPane = createGrid(0)

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)

  var currentStructuralChromosome: Map[String, CustomChromosomeInfo] = currentAnimalChromosome.structuralChromosome

  var properties: Map[String, Class[_]] = if (gene.isDefined) currentStructuralChromosome(gene.get).geneInfo.properties else Map.empty
  var conversionMap: Map[String, Map[String, Double]] = if (gene.isDefined) currentStructuralChromosome(gene.get).geneInfo.conversionMap else Map.empty

  val propertiesName: ObservableBuffer[String] = ObservableBuffer[String](properties.keySet toSeq)
  val propertiesListView: ListView[String] = new ListView[String] {
    items = propertiesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        PropertiesDialog(window, animal, gene,  Some(value), if (conversionMap.isEmpty) None else Some(conversionMap(value)), propertiesName).showAndWait() match {
          case Some(ConversionMap(propertyName, map)) =>
            conversionMap += (propertyName -> map)
          case None => println("Dialog returned: None")
        }
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  propertiesListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT

  val propertiesButton = new Button("Add")
  propertiesButton.onAction = _ => PropertiesDialog(window, animal, None, None, None, propertiesName).showAndWait() match {
    case Some(ConversionMap(propertyName, map)) =>
      conversionMap += (propertyName -> map)
      properties += (propertyName -> Double.getClass)
      propertiesName.insert(propertiesName.size, propertyName)
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
  Checks
   */


  val genes: Map[String, ChromosomeInfo] = currentAnimalChromosome.structuralChromosome ++
    currentAnimalChromosome.regulationChromosome ++
    currentAnimalChromosome.sexualChromosome

  val genesName: Set[String] = genes.keySet ++
    (RegulationDefaultGenes.elements ++ SexualDefaultGenes.elements).map(x => x.name)

  mandatoryFields = fields.keySet
  listFields = Seq(propertiesName)
  uniqueFields = Map(
    nameGene -> genesName,
    idGene -> genes.values.map(x => x.geneInfo.id).toSet
  )
  lengthFields = Map(idGene -> EntitiesInfo.instance().getAnimalBaseInfo(animal).geneLength)

  createChecks()


  /*
  Restart information
  */

  if (gene.isDefined) {
    nameGene.editable = false
    nameGene.text.value = currentStructuralChromosome(gene.get).geneInfo.name
    idGene.editable = false
    idGene.text.value = currentStructuralChromosome(gene.get).geneInfo.id
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

}


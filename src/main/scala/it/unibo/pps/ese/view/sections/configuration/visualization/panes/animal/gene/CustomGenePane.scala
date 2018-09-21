package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene



import it.unibo.pps.ese.controller.simulation.loader.{DefaultGene, RegulationDefaultGenes, SexualDefaultGenes}
import it.unibo.pps.ese.view.sections.configuration.visualization.panes._
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.ChromosomePane
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.alleles.AllelesPane
import it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.customproperties.PropertiesPane
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{CustomListView, ErrorLabel, WhiteLabel}
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo._
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.support.animals._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.{AddModality, MainDialog, Modality, ModifyModality}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, Pane, VBox}
import scalafx.stage.Window

object CustomGeneProperties {
  val title = "Custom Gene Pane"
  val headerText = "Define your chromosome"
}

import CustomGeneProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

case class CustomGenePane(mainDialog: MainDialog,
                          override val previousContent: Option[ChromosomePane],
                          modality: Modality,
                          animal: String,
                          gene: Option[String] = None)
  extends GenePane(mainDialog, previousContent, gene, title, headerText, previousContent.get.path + newLine(3) + title) {

  /*
  Header
   */

//  title = "Custom Gene Dialog"
//  headerText = "Define structural chromosome"

  /*
  Fields
   */

  val idGene: TextField = new TextField()
  val nameGene: TextField = new TextField()

  fields = ListMap(
    idGene -> (new WhiteLabel("Id"), new ErrorLabel("")),
    nameGene -> (new WhiteLabel("Name"), new ErrorLabel(""))
  )

  val grid: GridPane = createGrid(0)

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)

  var currentStructuralChromosome: Map[String, CustomChromosomeInfo] = currentAnimalChromosome.structuralChromosome

  var properties: Map[String, Class[_]] = if (gene.isDefined) currentStructuralChromosome(gene.get).geneInfo.properties else Map.empty
  var conversionMap: Map[String, Map[String, Double]] = if (gene.isDefined) currentStructuralChromosome(gene.get).geneInfo.conversionMap else Map.empty

  val propertiesName: ObservableBuffer[String] = ObservableBuffer[String](properties.keySet toSeq)
  val propertiesListView: ListView[String] = new CustomListView[String] {
    items = propertiesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(PropertiesPane(mainDialog, Some(CustomGenePane.this), ModifyModality, animal, gene,
          Some(value), if (conversionMap.isEmpty) None else Some(conversionMap(value)), propertiesName))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }


  val propertiesButton = new Button("Add")
  propertiesButton.onAction = _ => mainDialog.setContent(PropertiesPane(mainDialog, Some(this), AddModality, animal, None, None, None, propertiesName))



  val propertiesPane = new BorderPane()
  propertiesPane.left = new WhiteLabel("Properties")
  propertiesPane.right = propertiesButton
  propertiesPane.margin = Insets(30,0,0,0)

  center = new VBox() {
    children ++= Seq(grid, propertiesPane, propertiesListView,  new WhiteLabel("At least one property"))
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

  okButton.onAction = _ => {
    EntitiesInfo.instance().setChromosomeBaseInfo(animal, StructuralChromosome,
      CustomGeneInfo(idGene.text.value, nameGene.text.value, properties, conversionMap))
    mainDialog.setContent(AllelesPane(mainDialog, Some(this), animal, nameGene.text.value, StructuralChromosome))
  }



  override def confirmAlleles(gene: String): Unit = {
    println(modality)
    previousContent.get.confirmStructuralChromosome(modality, gene)
  }


  def confirmProperties(modality: Modality, c: ConversionMap): Unit = {
    modality match {
      case AddModality =>
        conversionMap += (c.property -> c.map)
        properties += (c.property -> Double.getClass)
        propertiesName.insert(propertiesName.size, c.property)
      case ModifyModality =>
        conversionMap += (c.property -> c.map)
    }
    mainDialog.setContent(this)

  }

}


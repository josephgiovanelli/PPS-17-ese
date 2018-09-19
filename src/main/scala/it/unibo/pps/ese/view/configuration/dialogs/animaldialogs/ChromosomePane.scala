package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs


import it.unibo.pps.ese.controller.loader._
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.{CustomGenePane, DefaultGenePane}
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals.AnimalChromosomeInfo
import it.unibo.pps.ese.view.configuration.entitiesinfo.{ChromosomeTypes, EntitiesInfo}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, Pane, VBox}
import scalafx.stage.Window

case class ChromosomePane(mainDialog: MainDialog,
                          override val previousContent: Option[AnimalPane],
                          modality: Modality,
                          animal: String) extends BackPane(mainDialog, previousContent, None) {

  /*
  Header
   */

  mainDialog.title = "Chromosome Dialog"
  mainDialog.headerText = "Define animal chromosome"

  /*
  Fields
   */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)

  val structuralName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.structuralChromosome.keySet toSeq)
  val structuralChromosomeListView: ListView[String] = new ListView[String] {
    items = structuralName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(CustomGenePane(mainDialog, Some(ChromosomePane.this), ModifyModality, animal, Some(value)))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val regulationName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.regulationChromosome.keySet toSeq)
  val regulationChromosomeListView: ListView[String] = new ListView[String] {
    items = regulationName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(DefaultGenePane(mainDialog, Some(ChromosomePane.this), ModifyModality, ChromosomeTypes.REGULATION, animal, Some(value),
          RegulationDefaultGenes.elements -- getCurrentRegulationChromosome))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val sexualName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.sexualChromosome.keySet toSeq)
  val sexualChromosomeListView: ListView[String] = new ListView[String] {
    items = sexualName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(DefaultGenePane(mainDialog, Some(ChromosomePane.this), ModifyModality, ChromosomeTypes.SEXUAL, animal, Some(value),
          SexualDefaultGenes.elements -- getCurrentSexualChromosome))
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

//  structuralChromosomeListView.prefHeight = MIN_ELEM * ROW_HEIGHT
//  regulationChromosomeListView.prefHeight =  MIN_ELEM *  ROW_HEIGHT
//  sexualChromosomeListView.prefHeight =  MIN_ELEM *  ROW_HEIGHT

  val structuralButton = new Button("Add")
  structuralButton.onAction = _ => mainDialog.setContent(CustomGenePane(mainDialog, Some(ChromosomePane.this), AddModality, animal, None))

  val regulationButton = new Button("Add")
  regulationButton.onAction = _ => {
    mainDialog.setContent(DefaultGenePane(mainDialog, Some(ChromosomePane.this), AddModality, ChromosomeTypes.REGULATION, animal, None,
      RegulationDefaultGenes.elements -- getCurrentRegulationChromosome))
  }

  val sexualButton = new Button("Add")
  sexualButton.onAction = _ => {
    mainDialog.setContent(DefaultGenePane(mainDialog, Some(this), AddModality, ChromosomeTypes.SEXUAL, animal, None,
      SexualDefaultGenes.elements -- getCurrentSexualChromosome))
  }

  val structuralPane = new BorderPane()
  structuralPane.left = new Label("Structural Chromosome")
  structuralPane.right = structuralButton


  val regulationPane = new BorderPane()
  regulationPane.left = new Label("Regulation Chromosome")
  regulationPane.right = regulationButton

  val sexualPane = new BorderPane()
  sexualPane.left = new Label("Sexual Chromosome")
  sexualPane.right = sexualButton

  center = new VBox() {
    children ++= Seq(structuralPane, structuralChromosomeListView, regulationPane, regulationChromosomeListView,
      sexualPane, sexualChromosomeListView, new Label("At least one element per chromosome"))
    styleClass += "sample-page"
  }

  /*
  Checks
   */

  listFields = Seq(structuralName, regulationName, sexualName)

  createChecks()

  /*
  Support Methods
  */

  private def getCurrentRegulationChromosome: Set[RegulationDefaultGene] =
    currentAnimalChromosome.regulationChromosome.keySet.map(x => RegulationDefaultGenes.elements.filter(y => y.name.equals(x)).head)

  private def getCurrentSexualChromosome: Set[SexualDefaultGene] =
    currentAnimalChromosome.sexualChromosome.keySet.map(x => SexualDefaultGenes.elements.filter(y => y.name.equals(x)).head)

  okButton.onAction = _ => {
    previousContent.get.confirmChromosome(animal)
  }


  def confirmStructuralChromosome(m: Modality, name: String): Unit = {
    m match {
      case AddModality =>
        structuralName.insert(structuralName.size, name)
      case ModifyModality =>
        Platform.runLater(structuralChromosomeListView.selectionModel().clearSelection())
    }
    mainDialog.setContent(this)
  }

  def confirmRegulationChromosome(m: Modality, name: String): Unit = {
    m match {
      case AddModality =>
        regulationName.insert(regulationName.size, name.toString)
        regulationButton.disable = (RegulationDefaultGenes.elements -- getCurrentRegulationChromosome).isEmpty
      case ModifyModality =>
        Platform.runLater(regulationChromosomeListView.selectionModel().clearSelection())
    }
    mainDialog.setContent(this)
  }

  def confirmSexualChromosome(m: Modality, name: String): Unit = {
    m match {
      case AddModality =>
        sexualName.insert(sexualName.size, name.toString)
        sexualButton.disable = (SexualDefaultGenes.elements -- getCurrentSexualChromosome).isEmpty
      case ModifyModality =>
        Platform.runLater(regulationChromosomeListView.selectionModel().clearSelection())

    }
    mainDialog.setContent(this)
  }


}

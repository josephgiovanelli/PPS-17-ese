package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.{CustomGeneDialog, DefaultGeneDialog}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.Window

case class ChromosomeDialog(window: Window, animal: String) extends Dialog {

  /*
  Header
   */

  initOwner(window)
  title = "Chromosome Dialog"
  headerText = "Define animal chromosome"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")


  /*
  Fields
   */

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo
    case None => throw new IllegalStateException()
  }

  val structuralName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.structuralChromosome.keySet toSeq)
  val structuralChromosomeListView: ListView[String] = new ListView[String] {
    items = structuralName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        CustomGeneDialog(window, animal, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  val regulationName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.regulationChromosome.keySet toSeq)
  val regulationChromosomeListView: ListView[String] = new ListView[String] {
    items = regulationName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        DefaultGeneDialog(window, ChromosomeTypes.REGULATION, animal, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }


  val sexualName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.sexualChromosome.keySet toSeq)
  val sexualChromosomeListView: ListView[String] = new ListView[String] {
    items = sexualName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        DefaultGeneDialog(window, ChromosomeTypes.SEXUAL, animal, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  structuralChromosomeListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT
  regulationChromosomeListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT
  sexualChromosomeListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT

  val structuralButton = new Button("Add")
  structuralButton.onAction = _ => CustomGeneDialog(window, animal, None).showAndWait() match {
    case Some(name) =>
      structuralName.insert(structuralName.size, name.toString)
    case None => println("Dialog returned: None")
  }
  val regulationButton = new Button("Add")
  regulationButton.onAction = _ => DefaultGeneDialog(window, ChromosomeTypes.REGULATION, animal, None).showAndWait() match {
    case Some(name) =>
      regulationName.insert(regulationName.size, name.toString)
    case None => println("Dialog returned: None")
  }
  val sexualButton = new Button("Add")
  sexualButton.onAction = _ => DefaultGeneDialog(window, ChromosomeTypes.SEXUAL, animal, None).showAndWait() match {
    case Some(name) =>
      sexualName.insert(sexualName.size, name.toString)
    case None => println("Dialog returned: None")
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

  dialogPane().content = new VBox() {
    children ++= Seq(structuralPane, structuralChromosomeListView, regulationPane, regulationChromosomeListView,
      sexualPane, sexualChromosomeListView, new Label("At least one element per chromosome"))
    styleClass += "sample-page"
  }

  /*
  OkButton
  */

  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  val mandatoryFields = Seq(structuralName, regulationName, sexualName)

  mandatoryFields.foreach(subject =>
    subject.onChange ((_, _) =>
      okButton.disable = checkField))

  def checkField: Boolean = mandatoryFields.exists(x => x.isEmpty)

}

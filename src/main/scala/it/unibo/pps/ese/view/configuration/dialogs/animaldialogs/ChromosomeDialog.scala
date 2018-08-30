package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.plantdialogs.PlantDialog
import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.CustomGeneDialog

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.Window

case class ChromosomeDialog(window: Window, animal: String) extends Dialog {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)
  title = "Chromosome Dialog"
  headerText = "Define animal chromosome"

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

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
        PlantDialog(window, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }


  val sexualName: ObservableBuffer[String] = ObservableBuffer[String](currentAnimalChromosome.sexualChromosome.keySet toSeq)
  val sexualChromosomeListView: ListView[String] = new ListView[String] {
    items = sexualName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        PlantDialog(window, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  structuralChromosomeListView.prefHeight = MIN_ELEM * ROW_HEIGHT
  regulationChromosomeListView.prefHeight = MIN_ELEM * ROW_HEIGHT
  sexualChromosomeListView.prefHeight = MIN_ELEM * ROW_HEIGHT

  val structuralButton = new Button("Add")
  structuralButton.onAction = _ => CustomGeneDialog(window, animal, None).showAndWait() match {
    case Some(name) => {
      structuralName.insert(structuralName.size, name.toString)
    }
    case None => println("Dialog returned: None")
  }
  val regulationButton = new Button("Add")
  regulationButton.onAction = _ => PlantDialog(window).showAndWait() match {
    case Some(name) => {
      regulationName.insert(regulationName.size, name.toString)
    }
    case None => println("Dialog returned: None")
  }
  /*regulationButton.onAction = _ => {
    val effect: Map[String, Double] = Map("life" -> 2)
    val aaa = Allele("aaa", "zzz", 5, 5, 1, effect)
    val life = DefaultGeneInfo(RegulationDefaultGenes.LIFE, "aaa", Set(aaa))
    regulationChromosome ++= Seq(life)
    println(regulationChromosome.size)
    regulationName.insert(0, life.name)
  }*/
  val sexualButton = new Button("Add")
  sexualButton.onAction = _ => PlantDialog(window).showAndWait() match {
    case Some(name) => {
      sexualName.insert(sexualName.size, name.toString)
    }
    case None => println("Dialog returned: None")
  }
  /*sexualButton.onAction = _ => {
    LoginDialog(window).showAndWait() match {
      case Some(Result(a, b)) => println((a, b))
      case None => println("ciao")
    }
  }*/

  val structuralPane = new BorderPane()
  structuralPane.left = new Label("Structural Chromosome")
  structuralPane.right = structuralButton

  val regulationPane = new BorderPane()
  regulationPane.left = new Label("Regulation Chromosome")
  regulationPane.right = regulationButton

  val sexualPane = new BorderPane()
  sexualPane.left = new Label("Sexual Chromosome")
  sexualPane.right = sexualButton


  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(structuralPane, structuralChromosomeListView, regulationPane, regulationChromosomeListView,
      sexualPane, sexualChromosomeListView)
    styleClass += "sample-page"
  }


}

package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.AlleleData
import it.unibo.pps.ese.view.configuration.dialogs._

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.Window

case class AllelesDialog(window: Window, animal: String, gene: String, chromosomeTypes: ChromosomeTypes.Value) extends Dialog[Unit] {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)
  title = "Alleles Dialog"
  headerText = "Define chromosome alleles"

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  var currentAlleles: Map[String, AlleleData] = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo.structuralChromosome.get(gene) match {
      case Some((_, alleles)) => alleles
      case None => throw new IllegalStateException()

    }
    case None => throw new IllegalStateException()
  }

  val allelesName: ObservableBuffer[String] = ObservableBuffer[String](currentAlleles.keySet toSeq)
  val allelesListView: ListView[String] = new ListView[String] {
    items = allelesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        AlleleDialog(window, animal, gene, Some(value)).showAndWait() match {
          case Some(Allele(alleleGene, id, dominance, consume, probability, effect)) => {
            currentAlleles += (id -> Allele(alleleGene, id, dominance, consume, probability, effect))
            EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
          }
          case None => println("Dialog returned: None")
        }
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }



  allelesListView.prefHeight = MIN_ELEM * ROW_HEIGHT

  val allelesButton = new Button("Add")
  allelesButton.onAction = _ => AlleleDialog(window, animal, gene, None).showAndWait() match {
    case Some(Allele(alleleGene, id, dominance, consume, probability, effect)) => {
      currentAlleles += (id -> Allele(alleleGene, id, dominance, consume, probability, effect))
      allelesName.insert(allelesName.size, id)
      EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
    }
    case None => println("Dialog returned: None")
  }

  val allelesPane = new BorderPane()
  allelesPane.left = new Label("Alleles")
  allelesPane.right = allelesButton


  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(allelesPane, allelesListView)
    styleClass += "sample-page"
  }

}


package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs

import javafx.scene.Node

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.AlleleData
import it.unibo.pps.ese.view.configuration.dialogs.EntitiesInfo
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog.ConversionMapDialog

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.stage.Window

case class AlleleDialog(window: Window, animal: String, gene: String, currentIdAllele: Option[String]) extends Dialog[Allele] {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)
  title = "Allele Dialog"
  headerText = "Create an allele"

  var currentAlleles: Map[String, AlleleData] = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo.structuralChromosome.get(gene) match {
      case Some((_, alleles)) => alleles
      case None => throw new IllegalStateException()
    }
    case None => throw new IllegalStateException()
  }

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  // Create the username and password labels and fields.
  val idAllele: TextField = new TextField() {
    promptText = "Id"
  }
  val dominance: TextField = new TextField() {
    promptText = "Dominance"
  }
  val consume: TextField = new TextField() {
    promptText = "Consume"
  }
  val probability: TextField = new TextField() {
    promptText = "Probability"
  }

  val requiredField = Seq(idAllele, dominance, consume, probability)

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Id"), 0, 0)
    add(idAllele, 1, 0)
    add(new Label("Dominance"), 0, 1)
    add(dominance, 1, 1)
    add(new Label("Consume"), 0, 2)
    add(consume, 1, 2)
    add(new Label("Probability"), 0, 3)
    add(probability, 1, 3)
  }


  var effects:  Map[String, Double] =
    if (currentIdAllele.isDefined) currentAlleles(currentIdAllele.get).effect
    else Map.empty
  val effectsName: ObservableBuffer[String] = ObservableBuffer[String](effects.keySet toSeq)
  val effectsListView: ListView[String] = new ListView[String] {
    items = effectsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        EffectDialog(window, Some((value, effects(value)))).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  effectsListView.prefHeight = MIN_ELEM * ROW_HEIGHT

  val effectsButton = new Button("Add")
  effectsButton.onAction = _ => EffectDialog(window, None).showAndWait() match {
    case Some((name: String, value: Double)) =>
      effects += (name -> value)
      effectsName.insert(effectsName.size, name)
    case None => println("Dialog returned: None")
  }

  val effectsPane = new BorderPane()
  effectsPane.left = new Label("Conversion Map")
  effectsPane.right = effectsButton


  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  requiredField.foreach(subject => {
    subject.text.onChange { (_, _, newValue) =>
      okButton.disable = newValue.trim().isEmpty || requiredField.filter(x => !x.equals(subject)).exists(x => x.getText.trim().isEmpty)
    }
  })

  dialogPane().content = new VBox() {
    children ++= Seq(grid, effectsPane, effectsListView)
    styleClass += "sample-page"
  }
  // Request focus on the username field by default.
  Platform.runLater(idAllele.requestFocus())

  // When the login button is clicked, convert the result to
  // a username-password-pair.

  if (currentIdAllele.isDefined) {
    idAllele.editable = false
    idAllele.text.value = currentAlleles(currentIdAllele.get).id
    dominance.text.value = currentAlleles(currentIdAllele.get).dominance.toString
    consume.text.value = currentAlleles(currentIdAllele.get).consume.toString
    probability.text.value = currentAlleles(currentIdAllele.get).probability.toString
  }

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType)
      Allele(gene, idAllele.text.value, dominance.text.value.toDouble, consume.text.value.toDouble, probability.text.value.toDouble, effects)
    else
      null

}


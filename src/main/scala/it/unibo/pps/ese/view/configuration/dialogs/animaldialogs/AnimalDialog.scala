package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.ConfigurationView
import it.unibo.pps.ese.view.configuration.dialogs.{AnimalBaseInfo, EntitiesInfo}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

case class AnimalDialog(window: Window, key: Option[String] = None) extends Dialog[String] {
  initOwner(window)
  title = "Animal Dialog"
  headerText = "Create an animal"

  // Set the button types.
  val okButtonType = new ButtonType("Insert Chromosome", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  // Create the username and password labels and fields.
  val name: TextField = new TextField() {
    promptText = "Name"
  }
  val geneLength: TextField = new TextField() {
    promptText = "Gene Length"
  }
  val alleleLength: TextField = new TextField() {
    promptText = "Allele Length"
  }
  val reign: TextField = new TextField() {
    promptText = "Reign"
  }
  val typology: TextField = new TextField() {
    promptText = "Typology"
  }

  val requiredField = Seq(name, geneLength, alleleLength, reign, typology)

  val grid = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Name"), 0, 0)
    add(name, 1, 0)
    add(new Label("Gene Length"), 0, 1)
    add(geneLength, 1, 1)
    add(new Label("Allele Length"), 0, 2)
    add(alleleLength, 1, 2)
    add(new Label("Reign"), 0, 3)
    add(reign, 1, 3)
    add(new Label("Typology"), 0, 4)
    add(typology, 1, 4)
  }

  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  requiredField.foreach(subject => {
    subject.text.onChange { (_, _, newValue) =>
      okButton.disable = newValue.trim().isEmpty || requiredField.filter(x => !x.equals(subject)).exists(x => x.getText.trim().isEmpty)
    }
  })

  dialogPane().content = grid

  // Request focus on the username field by default.
  Platform.runLater(name.requestFocus())

  // When the login button is clicked, convert the result to
  // a username-password-pair.

  if (key.isDefined) {
    val animalInfo = EntitiesInfo.instance().getAnimalInfo(key.get) match {
      case Some((basicInfo, _)) => basicInfo
      case None => throw new IllegalStateException()
    }
    name.editable = false
    name.text.value = key.get
    geneLength.text.value = animalInfo.geneLength.toString
    alleleLength.text.value = animalInfo.alleleLength.toString
    reign.text.value = animalInfo.reign.toString
    typology.text.value = animalInfo.typology.toString
  }

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      EntitiesInfo.instance().setAnimalBaseInfo(name.text.value, AnimalBaseInfo(geneLength.text.value.toInt, alleleLength.text.value.toInt, reign.text.value, typology.text.value))
      ChromosomeDialog(window, if (key.isEmpty) Some(name.text.value) else key).showAndWait()
      name.text.value
    }
    else
      null


  /*def showAndThenPrint() = {
    this.showAndWait() match {
      case Some(AnimalBaseInfo(n, g, a, r, t)) => {
        println("AnimalBaseInfo(" + n + ", " + g + ", " + a + ", " + r + ", " + t + ")")
        ChromosomeDialog(window).showAndWait() match {
          case Some(AnimalChromosomeInfo(st, re, se)) => {
            println("AnimalChromosomeInfo(" + st + ", " + re + ", " + se + ")")
          }
          case None => println("Dialog returned: None")
        }
      }
      case None => println("Dialog returned: None")
    }
  }*/

}

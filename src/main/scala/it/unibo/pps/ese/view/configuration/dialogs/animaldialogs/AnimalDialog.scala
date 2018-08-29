package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import it.unibo.pps.ese.view.configuration.ConfigurationView

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

case class AnimalDialog(window: Window) extends Dialog {
  initOwner(window)
  title = "Animal Dialog"
  headerText = "Create an animal"

  // Set the button types.
  val okButtonType = new ButtonType("Insert Chromosome", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  // Create the username and password labels and fields.
  val name = new TextField() {
    promptText = "Name"
  }
  val geneLength = new TextField() {
    promptText = "Gene Length"
  }
  val alleleLength = new TextField() {
    promptText = "Allele Length"
  }
  val reign = new TextField() {
    promptText = "Reign"
  }
  val tipology = new TextField() {
    promptText = "Tipology"
  }

  val requiredField = Seq(name, geneLength, alleleLength, reign, tipology)

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
    add(new Label("Tipology"), 0, 4)
    add(tipology, 1, 4)
  }

  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton = dialogPane().lookupButton(okButtonType)
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
  /*resultConverter = dialogButton =>
    if (dialogButton == okButtonType)
      AnimalBaseInfo(name.text.get, geneLength.text.get.toInt, alleleLength.text.get.toInt, reign.text.get, tipology.text.get)
    else
      null*/


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

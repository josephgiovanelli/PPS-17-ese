package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.{AnimalBaseInfo, EntitiesInfo}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.stage.Window

case class AnimalDialog(window: Window, animal: Option[String] = None) extends Dialog[String] {

  /*
  Header
   */

  initOwner(window)
  title = "Animal Dialog"
  headerText = "Create an animal"

  /*
  Fields
   */

  val name: TextField = new TextField()
  val geneLength: TextField = new TextField()
  val alleleLength: TextField = new TextField()
  val reign: TextField = new TextField()
  val typology: TextField = new TextField()
  val errorLabel = new Label()

  val fields: Map[TextField, Label] = ListMap(
    name -> new Label("Name"),
    geneLength -> new Label("Gene Length"),
    alleleLength -> new Label("Allele Length"),
    reign -> new Label("Reign"),
    typology -> new Label("Typology"),
  )

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    var count = 0
    fields.foreach(field => {
      add(field._2, 0, count)
      add(field._1, 1, count)
      count += 1
    })
  }

  dialogPane().content = new VBox(grid, errorLabel)

  Platform.runLater(name.requestFocus())

  /*
  OkButton
   */
  val okButtonType = new ButtonType("Insert Chromosome", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  /*
  Checks
   */

  val mandatoryFields: Set[TextField] = fields.keySet
  val error: StringProperty = StringProperty(checkFields())
  errorLabel.text <== error

  mandatoryFields.foreach(subject => {
    subject.text.onChange ( (_, _, newValue) =>{
      error.value = checkFields()
      okButton.disable = newValue.trim().isEmpty || mandatoryFields.filter(x => !x.equals(subject)).exists(x => x.getText.trim().isEmpty)
    })
  })

  /*
  Restart information
   */

  if (animal.isDefined) {
    val animalInfo = EntitiesInfo.instance().getAnimalInfo(animal.get) match {
      case Some((basicInfo, _)) => basicInfo
      case None => throw new IllegalStateException()
    }
    name.editable = false
    name.text.value = animal.get
    geneLength.text.value = animalInfo.geneLength.toString
    alleleLength.text.value = animalInfo.alleleLength.toString
    reign.text.value = animalInfo.reign.toString
    typology.text.value = animalInfo.typology.toString
  }

  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      EntitiesInfo.instance().setAnimalBaseInfo(name.text.value, AnimalBaseInfo(geneLength.text.value.toInt, alleleLength.text.value.toInt, reign.text.value, typology.text.value))
      ChromosomeDialog(window, if (animal.isEmpty) name.text.value else animal.get).showAndWait()
      name.text.value
    }
    else
      null



  private def checkFields(): String = {
    val mandatoryCheck = mandatoryFields.filter(x => x.getText.trim().isEmpty)
    var checksSuccessful = true
    var toPrint: String = ""
    if (mandatoryCheck.nonEmpty) {
      toPrint = "Empty fields: " + mandatoryCheck.map(field => fields(field).text.value).foldRight("")(_ + " | " + _) + "\n"
      checksSuccessful = false
    }
    toPrint
  }

}

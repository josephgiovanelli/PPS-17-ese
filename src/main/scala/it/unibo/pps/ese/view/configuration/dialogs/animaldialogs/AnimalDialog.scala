package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.{AnimalBaseInfo, EntitiesInfo, ParseUtils}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
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

  val typologySet: ObservableBuffer[String] = ObservableBuffer[String]("Carnivorous", "Herbivore")

  val name = new TextField()
  val geneLength = new TextField()
  val alleleLength = new TextField()
  val typology = new ComboBox(typologySet)
  typology.selectionModel().select(0)
  val errorLabel = new Label()

  val fields: Map[TextField, Label] = ListMap(
    name -> new Label("Name"),
    geneLength -> new Label("Gene Length"),
    alleleLength -> new Label("Allele Length"),
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

    add(new Label("Typology"), 0, count)
    add(typology, 1, count)
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
  val intField: Set[TextField] = Set(geneLength, alleleLength)
  val error: StringProperty = StringProperty(checkFields()._1)
  errorLabel.text <== error

  mandatoryFields.foreach(subject => {
    subject.text.onChange ( (_, _, _) => {
      val result = checkFields()
      error.value = result._1
      okButton.disable = !result._2
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
    typology.selectionModel().select(animalInfo.typology.toString)
  }

  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      EntitiesInfo.instance().setAnimalBaseInfo(name.text.value, AnimalBaseInfo(geneLength.text.value.toInt, alleleLength.text.value.toInt, typology.value.value))
      ChromosomeDialog(window, if (animal.isEmpty) name.text.value else animal.get).showAndWait()
      name.text.value
    }
    else
      null



  private def checkFields(): (String, Boolean) = {
    val mandatoryCheck = mandatoryFields.filter(x => x.getText.trim().isEmpty)
    val intCheck = intField.filter(x => ParseUtils.parse[Int](x.getText.trim()).isEmpty)
    var checksSuccessful = true
    var toPrint: String = ""
    if (mandatoryCheck.nonEmpty) {
      toPrint += "Empty fields: " + mandatoryCheck.map(field => fields(field).text.value).foldRight("")(_ + " | " + _) + "\n"
      checksSuccessful = false
    }
    if (intCheck.nonEmpty) {
      toPrint += "Int fields: " + intCheck.map(field => fields(field).text.value).foldRight("")(_ + " | " + _)
      checksSuccessful = false
    }
    (toPrint, checksSuccessful)
  }

}

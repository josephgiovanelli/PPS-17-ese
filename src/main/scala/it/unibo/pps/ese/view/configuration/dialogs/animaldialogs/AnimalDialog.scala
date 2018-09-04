package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs.{AnimalBaseInfo, EntitiesInfo, ParseUtils}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class AnimalDialog(window: Window, animal: Option[String] = None) extends Dialog[String] {

  /*
  Header
   */

  initOwner(window)
  title = "Animal Dialog"
  headerText = "Create an animal"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  /*
  Fields
   */

  val typologySet: ObservableBuffer[String] = ObservableBuffer[String]("Carnivorous", "Herbivore")

  val name = new TextField()
  val geneLength = new TextField()
  val alleleLength = new TextField()
  val typology = new ComboBox(typologySet)
  typology.selectionModel().select(0)

  val fields: Map[TextField, (Label, Label)] = ListMap(
    name -> (new Label("Name"), new Label("")),
    geneLength -> (new Label("Gene Length"), new Label("")),
    alleleLength -> (new Label("Allele Length"), new Label("")),
  )

  val grid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(20, 100, 10, 10)

    var count = 0
    fields.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })

    add(new Label("Typology"), 0, count)
    add(typology, 1, count)
  }

  dialogPane().content = grid

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
  val intFields: Set[TextField] = Set(geneLength, alleleLength)
  val uniqueFields: Set[TextField] = Set(name)

  mandatoryFields.foreach(subject => {
    subject.text.onChange ( (_, _, newValue) => {
      okButton.disable = checkFields(subject, newValue)
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



  private def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatoryCheck = field.getText.trim().isEmpty
    val intCheck = if (intFields.contains(field)) ParseUtils.parse[Int](field.getText.trim()).isEmpty else false
    val uniqueCheck = if (animal.isEmpty && uniqueFields.contains(field)) EntitiesInfo.instance().getPlants.contains(field.text.value) || EntitiesInfo.instance().getAnimals.contains(field.text.value)
                      else false

    if (mandatoryCheck || intCheck || uniqueCheck)
      field.pseudoClassStateChanged(errorClass, true)
    else
      field.pseudoClassStateChanged(errorClass, false)

    if (mandatoryCheck) fields(field)._2.text.value = "Must be filled"
    else if (intCheck) fields(field)._2.text.value = "Must be int"
    else if (uniqueCheck) fields(field)._2.text.value = "Must be unique"
    else fields(field)._2.text.value = ""

    checkFields
  }

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) ||
    intFields.exists(x => ParseUtils.parse[Int](x.getText.trim()).isEmpty) ||
    (uniqueFields.exists(x => EntitiesInfo.instance().getPlants.contains(x.text.value)) && animal.isEmpty)


}

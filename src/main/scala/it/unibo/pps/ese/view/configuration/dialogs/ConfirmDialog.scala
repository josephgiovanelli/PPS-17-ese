package it.unibo.pps.ese.view.configuration.dialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.MainComponent

import scalafx.Includes._
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class ConfirmDialog(window: Window, mainComponent: MainComponent) extends Dialog[Unit] {

  /*
  Header
   */

  initOwner(window)
  title = "Confirm Dialog"
  headerText = "Choose number of entities for each species"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")


  val animalsEntities: Map[TextField, (Label, Label)] =
    EntitiesInfo.instance().getAnimals.map(x => new TextField() -> (new Label(x), new Label(""))).groupBy(_._1).map{ case (k,v) =>
      (k,v.map(_._2))}.map(x => x._1 -> x._2.head)
  val plantsEntities: Map[TextField, (Label, Label)] =
    EntitiesInfo.instance().getPlants.map(x => new TextField() -> (new Label(x), new Label(""))).groupBy(_._1).map{ case (k,v) =>
      (k,v.map(_._2))}.map(x => x._1 -> x._2.head)

  val fields: Map[TextField, (Label, Label)] = animalsEntities ++ plantsEntities

  val animalsGrid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(10, 100, 10, 10)

    var count = 0
    animalsEntities.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
  }

  val animalsPane = new BorderPane()
  animalsPane.top = new Label("Animals")
  animalsPane.bottom = animalsGrid

  val plantsGrid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(10, 100, 10, 10)

    var count = 0
    plantsEntities.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
  }

  val plantsPane = new BorderPane()
  plantsPane.top = new Label("Plants")
  plantsPane.bottom = plantsGrid

  dialogPane().content = new VBox() {
    children ++= Seq(animalsPane, plantsPane)
    styleClass += "sample-page"
  }

  /*
  OkButton
   */

  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true


  /*
  Checks
   */

  val mandatoryFields: Set[TextField] = fields.keySet
  val doubleFields: Set[TextField] = mandatoryFields

  mandatoryFields.foreach(subject => {
    subject.text.onChange ( (_, _, newValue) => {
      okButton.disable = checkFields(subject, newValue)
    })
  })

  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      val animals = animalsEntities.map(animal => animal._2._1.text.value -> animal._1.text.value.toInt)
      val plants = plantsEntities.map(plant => plant._2._1.text.value -> plant._1.text.value.toInt)
      mainComponent.setUp(EntitiesInfo.instance().getSimulationData(animals, plants))
    }
    else
      null

  private def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatoryCheck = field.getText.trim().isEmpty
    val doubleCheck = if (doubleFields.contains(field)) ParseUtils.parse[Double](field.getText.trim()).isEmpty else false

    if (mandatoryCheck || doubleCheck)
      field.pseudoClassStateChanged(errorClass, true)
    else
      field.pseudoClassStateChanged(errorClass, false)

    if (mandatoryCheck) fields(field)._2.text.value = "Must be filled"
    else if (doubleCheck) fields(field)._2.text.value = "Must be double"
    else fields(field)._2.text.value = ""

    checkFields
  }

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) || doubleFields.exists(x => ParseUtils.parse[Double](x.getText.trim()).isEmpty)



}

package it.unibo.pps.ese.view.configuration.dialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.MainComponent

import scalafx.Includes._
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
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


  val animalsEntities: Map[String, TextField] =
    EntitiesInfo.instance().getAnimals.map(x => x -> new TextField()).groupBy(_._1).map{ case (k,v) =>
      (k,v.map(_._2))}.map(x => x._1 -> x._2.head)
  val plantsEntities: Map[String, TextField] =
    EntitiesInfo.instance().getPlants.map(x => x -> new TextField()).groupBy(_._1).map{ case (k,v) =>
      (k,v.map(_._2))}.map(x => x._1 -> x._2.head)

  val animalsGrid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    var currentRow = 0
    animalsEntities.foreach(animal => {
      add(new Label(animal._1), 0, currentRow)
      add(animal._2, 1, currentRow)
      currentRow += 1
    })
  }

  val animalsPane = new BorderPane()
  animalsPane.top = new Label("Animals")
  animalsPane.bottom = animalsGrid

  val plantsGrid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    var currentRow = 0
    plantsEntities.foreach(plant => {
      add(new Label(plant._1), 0, currentRow)
      add(plant._2, 1, currentRow)
      currentRow += 1
    })
  }

  val plantsPane = new BorderPane()
  plantsPane.top = new Label("Plants")
  plantsPane.bottom = plantsGrid

  val requiredField: Iterable[TextField] = animalsEntities.values ++ plantsEntities.values

  requiredField.foreach(subject => {
    subject.text.onChange { (_, _, newValue) =>
      okButton.disable = newValue.trim().isEmpty || requiredField.filter(x => !x.equals(subject)).exists(x => x.getText.trim().isEmpty)
    }
  })

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
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      val animals = animalsEntities.map(animal => animal._1 -> animal._2.text.value.toInt)
      val plants = plantsEntities.map(plant => plant._1 -> plant._2.text.value.toInt)
      mainComponent.setUp(EntitiesInfo.instance().getSimulationData(animals, plants))
    }
    else
      null


}

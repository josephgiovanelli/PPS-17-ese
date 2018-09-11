package it.unibo.pps.ese.view.configuration.dialogs


import it.unibo.pps.ese.view.MainComponent
import it.unibo.pps.ese.view.configuration.entitiesinfo.EntitiesInfo

import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class ConfirmDialog(window: Window, mainComponent: MainComponent, setUp: Boolean) extends AbstractDialog[Unit](window, None) {

  /*
  Header
   */

  title = "Confirm Dialog"
  headerText = "Choose number of entities for each species"


  val animalsEntities: Map[TextField, (Label, Label)] =
    EntitiesInfo.instance().getAnimals.map(x => new TextField() -> (new Label(x), new Label(""))).groupBy(_._1).map{ case (k,v) =>
      (k,v.map(_._2))}.map(x => x._1 -> x._2.head)
  val plantsEntities: Map[TextField, (Label, Label)] =
    EntitiesInfo.instance().getPlants.map(x => new TextField() -> (new Label(x), new Label(""))).groupBy(_._1).map{ case (k,v) =>
      (k,v.map(_._2))}.map(x => x._1 -> x._2.head)

  fields = animalsEntities ++ plantsEntities

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
  Checks
   */

  mandatoryFields = fields.keySet
  doubleFields = mandatoryFields

  createChecks()

  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) {
      val animals = animalsEntities.map(animal => animal._2._1.text.value -> animal._1.text.value.toInt)
      val plants = plantsEntities.map(plant => plant._2._1.text.value -> plant._1.text.value.toInt)
      if (setUp) {
        mainComponent.setUp(EntitiesInfo.instance().getSimulationData(animals, plants))
      } else {
        mainComponent.addEntities(animals, plants)
      }
    }
    else
      null

}

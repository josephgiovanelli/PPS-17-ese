package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.view.configuration.dialogs.{DefaultGeneInfo, LoginDialog}
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.AnimalDialog
import it.unibo.pps.ese.view.configuration.dialogs.plantdialogs.PlantDialog
import it.unibo.pps.ese.view.{MainComponent, ViewType}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.Window

trait ConfigurationView {
}

case class Result(username: String, password: String)

class ConfigurationViewImpl(mainComponent: MainComponent) extends Scene(250, 350) with ConfigurationView {


  val ROW_HEIGHT = 26
  val MIN_ELEM = 5

  val currentWindow: scalafx.stage.Window = this.window()

  /*val effect: Map[String, Double] = Map("life" -> 2)
  val aaa = Allele("aaa", "zzz", 5, 5, 1, effect)
  println(DefaultGeneInfo(RegulationDefaultGenes.LIFE, "aaa", Set(aaa)))*/

  /*val animalBeans = Animal("Gatto", 3, 3, "A", "C", "", null, null)
  val effect: Map[String, Double] = Map("life" -> 2)
  val aaa = Allele("aaa", "zzz", 5, 5, 1, effect)
  val life = DefaultGeneData(RegulationDefaultGenes.LIFE, "aaa", Seq(aaa))
  val chromosome = Seq(life)
  val animal = AnimalData(animalBeans, Iterable.empty, chromosome, Iterable.empty)
  val simulationDataImpl = SimulationDataImpl(Map(animal -> 0), Map())*/


  val animalsName: ObservableBuffer[String] = ObservableBuffer[String]()
  val animalsListView: ListView[String] = new ListView[String] {
    items = animalsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
        if (selectionModel().getSelectedIndex != -1) {
          AnimalDialog(currentWindow, Some(value)).showAndWait()
          Platform.runLater(selectionModel().clearSelection())
        }
      })
  }

  val plantsName: ObservableBuffer[String] = ObservableBuffer[String]()
  val plantsListView: ListView[String] = new ListView[String] {
    items = plantsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        PlantDialog(currentWindow, Some(value)).showAndWait()
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  animalsListView.prefHeight = MIN_ELEM * ROW_HEIGHT
  plantsListView.prefHeight = MIN_ELEM * ROW_HEIGHT

  val animalsAddButton = new Button("Add")
  val plantsAddButton = new Button("Add")
  animalsAddButton.onAction = _ => AnimalDialog(currentWindow).showAndWait() match {
    case Some(name) => {
      animalsName.insert(animalsName.size, name.toString)
    }
    case None => println("Dialog returned: None")
  }
  plantsAddButton.onAction = _ => PlantDialog(currentWindow).showAndWait() match {
    case Some(name) => {
      plantsName.insert(plantsName.size, name.toString)
    }
    case None => println("Dialog returned: None")
  }

  val animalsPane = new BorderPane()
  animalsPane.left = new Label("Animals")
  animalsPane.right = animalsAddButton

  val plantsPane = new BorderPane()
  plantsPane.left = new Label("Plants")
  plantsPane.right = plantsAddButton

  val confirmButton = new Button("Confirm")
  //confirmButton.onAction = _ =>  AnimalDialog(currentWindow).showAndThenPrint()


  content = new VBox() {
    children ++= Seq(animalsPane, animalsListView, plantsPane, plantsListView, confirmButton)
    styleClass += "sample-page"
  }



  //val result = createDialog1(currentWindow)

  /*result match {
    case Some(Result(u, p)) => {
      println("Username=" + u + ", Password=" + p)
      val result2 = createDialog2(currentWindow)

      result2 match {
        case Some(choice) => {
          println("Your choice: " + choice)
        }
        case None         => { println("No selection") }
      }
    }
    case None => println("Dialog returned: None")
  }*/






  //new Alert(AlertType.Information, "Hello Dialogs!!!").showAndWait()

  def createDialog1(window: Window) = {
    val dialog = LoginDialog(window)
    dialog.showAndWait()
  }

  def createDialog2(window: Window) = {
    val choices = Seq("a", "b", "c")
    val dialog2 = new ChoiceDialog(defaultChoice = "b", choices = choices) {
      initOwner(window)
      title = "Choice Dialog"
      headerText = "Look, a Choice Dialog."
      contentText = "Choose your letter:"
    }
    dialog2.showAndWait()
  }

  def prova(string: String): Unit = {

  }

}

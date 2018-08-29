package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.controller.loader.RegulationDefaultGenes
import it.unibo.pps.ese.controller.loader.beans.{Allele, Animal}
import it.unibo.pps.ese.controller.loader.data.{AnimalData, DefaultGeneData}
import it.unibo.pps.ese.controller.loader.data.SimulationData.SimulationDataImpl
import it.unibo.pps.ese.view.configuration.dialogs.LoginDialog
import it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.AnimalDialog
import it.unibo.pps.ese.view.{MainComponent, ViewType}

import scalafx.Includes._
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
  /*val dialog = LoginDialog(currentWindow)
  dialog.showAndWait()*/

  val animalBeans = Animal("Gatto", 3, 3, "A", "C", "", null, null)
  //simulationDataImpl.animals += (animal -> 0)


  val effect: Map[String, Double] = Map("life" -> 2)
  val aaa = Allele("aaa", "zzz", 5, 5, 1, effect)
  val life = DefaultGeneData(RegulationDefaultGenes.LIFE, "aaa", Seq(aaa))
  val chromosome = Seq(life)

  val animal = AnimalData(animalBeans, Iterable.empty, chromosome, Iterable.empty)

  val simulationDataImpl = SimulationDataImpl(Map(animal -> 0), Map())


  val choices = ObservableBuffer[String]("a", "b")

  val lista = new ListView[String] {
    items = choices
    selectionModel().selectedItem.onChange {
      (_, _, newValue) => println("Selection Changed: " + newValue)
    }
  }

  val choices2 = ObservableBuffer[String]("a", "b")

  val lista2 = new ListView[String] {
    items = choices2
    selectionModel().selectedItem.onChange {
      (_, _, newValue) => println("Selection Changed: " + newValue)
    }
  }

  lista.prefHeight = MIN_ELEM * ROW_HEIGHT
  lista2.prefHeight = MIN_ELEM * ROW_HEIGHT

  val button = new Button("Add")
  val button2 = new Button("Add")
  button.onAction = _ =>   choices.insert(choices.size, "d")
  button2.onAction = _ => choices2.insert(choices2.size, "d")

  val animalPane = new BorderPane()
  animalPane.left = new Label("Animal")
  animalPane.right = button

  val plantPane = new BorderPane()
  plantPane.left = new Label("Plant")
  plantPane.right = button2

  val confirmButton = new Button("Confirm")
  confirmButton.onAction = _ =>  AnimalDialog(currentWindow, this).showAndThenPrint()


  content = new VBox() {
    children ++= Seq(animalPane, lista, plantPane, lista2, confirmButton)
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

}

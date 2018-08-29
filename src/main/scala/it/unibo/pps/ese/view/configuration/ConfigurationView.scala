package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.view.{MainComponent, ViewType}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, HBox, VBox}
import scalafx.stage.Window

trait ConfigurationView {
}

case class Result(username: String, password: String)

class ConfigurationViewImpl(mainComponent: MainComponent) extends Scene(500, 500) with ConfigurationView {


  val ROW_HEIGHT = 26
  val MIN_HEIGHT = 3

  /*val currentWindow: scalafx.stage.Window = this.window()
  val dialog = LoginDialog(currentWindow)
  dialog.showAndWait()*/



  val choices = ObservableBuffer[String]("a", "b")

  val lista = new ListView[String] {
    items = choices
    selectionModel().selectedItem.onChange {
      (_, _, newValue) => println("Selection Changed: " + newValue)
    }
  }

  val choices2 = ObservableBuffer[(String, TextField)](("a", new TextField()), ("b", new TextField()))

  val lista2 = new ListView[(String, TextField)] {
    items = choices2
    selectionModel().selectedItem.onChange {
      (_, _, newValue) => println("Selection Changed: " + newValue)
    }
  }


  lista.prefHeight <== MIN_HEIGHT * ROW_HEIGHT
  lista2.prefHeight <== MIN_HEIGHT * ROW_HEIGHT

  val button = new Button("Add")
  val button2 = new Button("Add")
  button.onAction = _ =>   animals += ("f" -> new TextField())
  button2.onAction = _ => choices2.insert(2, ("d", new TextField()))

  val animalPane = new BorderPane()
  animalPane.left = new Label("Animal")
  animalPane.right = button

  val plantPane = new BorderPane()
  plantPane.left = new Label("Plant")
  plantPane.right = button2

  var animals: Map[String, TextField] = Map.empty
  animals += ("a" -> new TextField())
  animals += ("b" -> new TextField())
  animals += ("c" -> new TextField())
  animals += ("d" -> new TextField())
  animals += ("e" -> new TextField())

  val expContent = new GridPane {
    maxWidth = 100
    maxHeight = 50
    var count = 0
    animals.toIndexedSeq.foreach(animal => {
      add(new Label(animal._1), 0, count)
      add(animal._2, 1, count)
      count += 1
    })
  }

  val vBox : VBox = new VBox()
  vBox.children = animals map (animal => new HBox(new Label(animal._1), animal._2))

  val scrollPane = new ScrollPane {
    prefHeight = 500
    prefWidth = 500
    content = vBox
    styleClass += "noborder-scroll-pane"
  }


  content = new ScrollPane {
    prefHeight = 500
    prefWidth = 500
    content = new VBox() {
      children ++= Seq(animalPane, lista, plantPane, lista2)
      styleClass += "sample-page"
    }
    styleClass += "noborder-scroll-pane"
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

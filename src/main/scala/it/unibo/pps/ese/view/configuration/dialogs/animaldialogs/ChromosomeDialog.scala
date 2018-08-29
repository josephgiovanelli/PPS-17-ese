package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs

import it.unibo.pps.ese.controller.loader.RegulationDefaultGenes
import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.{CustomGeneData, DefaultGeneData}
import it.unibo.pps.ese.view.configuration.{ConfigurationView, Result}
import it.unibo.pps.ese.view.configuration.dialogs.{AnimalChromosomeInfo, LoginDialog}

import scala.collection.mutable.ListBuffer
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.Window

case class ChromosomeDialog(window: Window) extends Dialog[AnimalChromosomeInfo] {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)
  title = "Animal Dialog"
  headerText = "Create an animal"

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)

  val structuralChromosome: ListBuffer[CustomGeneData] = ListBuffer()
  val structuralName = ObservableBuffer[String]()
  val structuralChromosomeList = new ListView[String](structuralName)

  val regulationChromosome: ListBuffer[DefaultGeneData] = ListBuffer()
  val regulationName = ObservableBuffer[String]()
  val regulationChromosomeList = new ListView[String](regulationName)


  val sexualChromosome: ListBuffer[DefaultGeneData] = ListBuffer()
  val sexualName = ObservableBuffer[String]()
  val sexualChromosomeList = new ListView[String](sexualName)

  structuralChromosomeList.prefHeight = MIN_ELEM * ROW_HEIGHT
  regulationChromosomeList.prefHeight = MIN_ELEM * ROW_HEIGHT
  sexualChromosomeList.prefHeight = MIN_ELEM * ROW_HEIGHT

  val button = new Button("Add")
  val button2 = new Button("Add")
  button2.onAction = _ => {
    val effect: Map[String, Double] = Map("life" -> 2)
    val aaa = Allele("aaa", "zzz", 5, 5, 1, effect)
    val life = DefaultGeneData(RegulationDefaultGenes.LIFE, "aaa", Seq(aaa))
    regulationChromosome += life
    regulationName.insert(0, life.name)
  }
  val button3 = new Button("Add")
  button3.onAction = _ => {
    LoginDialog(window).showAndWait() match {
      case Some(Result(a, b)) => println((a, b))
      case None => println("ciao")
    }
  }

  val structuralPane = new BorderPane()
  structuralPane.left = new Label("Structural Chromosome")
  structuralPane.right = button

  val regulationPane = new BorderPane()
  regulationPane.left = new Label("Regulation Chromosome")
  regulationPane.right = button2

  val sexualPane = new BorderPane()
  sexualPane.left = new Label("Sexual Chromosome")
  sexualPane.right = button3


  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(structuralPane, structuralChromosomeList, regulationPane, regulationChromosomeList, sexualPane, sexualChromosomeList)
    styleClass += "sample-page"
  }


  // When the login button is clicked, convert the result to
  // a username-password-pair.
  resultConverter = dialogButton =>
    if (dialogButton == okButtonType)
      AnimalChromosomeInfo(structuralChromosome, regulationChromosome, sexualChromosome)
    else
      null

  def showAndPrint() = {
    this.showAndWait() match {
      case Some(AnimalChromosomeInfo(st, re, se)) => {
        println("AnimalChromosomeInfo(" + st + ", " + re + ", " + se + ")")
      }
      case None => println("Dialog returned: None")
    }
  }
}

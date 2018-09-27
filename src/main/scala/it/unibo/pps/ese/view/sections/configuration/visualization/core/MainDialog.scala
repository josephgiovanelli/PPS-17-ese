package it.unibo.pps.ese.view.sections.configuration.visualization.core

import it.unibo.pps.ese.view.sections.configuration.visualization.panes.{ConfigurationPane, ConfirmPane}
import it.unibo.pps.ese.view.core.{MainComponent, SetupViewBridge}
import it.unibo.pps.ese.view.sections.configuration.entitiesinfo.EntitiesInfo

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{ButtonType, Dialog, Label}
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.stage.Window

/**
  * The dialog where to put the content to be visualized
  */
trait MainDialog {
  /**
    *
    * @return the title of the dialog
    */
  def title: StringProperty

  /**
    * Sets the title of the dialog
    * @param title
    */
  def title_=(title: String)

  /**
    *
    * @return the header text of the dialog
    */
  def headerText: StringProperty

  /**
    * Sets the header text of the dialog
    * @param headerText
    */
  def headerText_=(headerText: String)

  /**
    * Sets the content of the dialog
    * @param content the `DialogPane` to be set
    */
  def setContent(content: DialogPane)

  /**
    * Shows the dialog
    */
  def show()

  /**
    *
    * @return the `Window` of this dialog
    */
  def window: Window

  /**
    * Closes the dialog
    */
  def closeDialog()

  /**
    * Adds the animal to the animal pending list
    * @param id the id of the animal
    */
  def addToPendingAnimals(id: String): Unit

  /**
    * Deletes the animal from the animal pending list
    * @param id the id of the animal
    */
  def deleteFromPendingAnimals(id: String): Unit

  /**
    * Cleans the animal pending list
    */
  def cleanPendingAnimals(): Unit
}

/**
  * The first content of the dialog
  */
trait FirstContent
case object ConfigurationContent extends FirstContent
case object ConfirmContent extends FirstContent

object MainDialog {

  def apply(window: Window,
            mainComponent: Option[MainComponent],
            setupViewBridge: Option[SetupViewBridge],
            setUp: Boolean,
            firstContent: FirstContent,
            newAnimalSpecies: Seq[String] = Seq.empty,
            newPlantSpecies: Seq[String] = Seq.empty,
            previousAnimalsCount: Map[String, Int] = Map.empty,
            previousPlantsCount: Map[String, Int] = Map.empty)
           (implicit executionContext: ExecutionContext): MainDialog =
    new MainDialogImpl(window, mainComponent, setupViewBridge, setUp, firstContent, newAnimalSpecies,
      newPlantSpecies, previousAnimalsCount, previousPlantsCount)

  class MainDialogImpl(val window: Window,
                       mainComponent: Option[MainComponent],
                       setupViewBridge: Option[SetupViewBridge],
                       setUp: Boolean,
                       firstContent: FirstContent,
                       newAnimalSpecies: Seq[String] = Seq.empty,
                       newPlantSpecies: Seq[String] = Seq.empty,
                       previousAnimalsCount: Map[String, Int] = Map.empty,
                       previousPlantsCount: Map[String, Int] = Map.empty)
                      (implicit executionContext: ExecutionContext) extends Dialog[Unit] with MainDialog {


    initOwner(window)

    val cancelCloseButtonType: ButtonType = new ButtonType("Exit", ButtonData.CancelClose)
    dialogPane().buttonTypes = Seq(cancelCloseButtonType)
    val cancelCloseButton: Node = dialogPane().lookupButton(cancelCloseButtonType)
    cancelCloseButton.visible = false

    dialogPane().background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))


    var pendingAnimals: ListBuffer[String] =  ListBuffer.empty
    private var currentContent: Option[DialogPane] = None
    val configurationPane = ConfigurationPane(this, None, setupViewBridge, mainComponent, setUp, previousAnimalsCount, previousPlantsCount)
    val confirmPane = ConfirmPane(
      this,
      None,
      setupViewBridge,
      mainComponent,
      setUp,
      newAnimalSpecies,
      newPlantSpecies,
      previousAnimalsCount,
      previousPlantsCount)

    dialogPane().content = firstContent match {
      case ConfigurationContent => configurationPane
      case ConfirmContent => confirmPane
    }

    def setContent(content: DialogPane): Unit = {
      dialogPane().content = content

      dialogPane().header = new BorderPane {

        center = new Label{
          margin = Insets(20, 0, 20, 0)
          text = content.headerText
          style = "-fx-text-fill:white;" +
            "-fx-font-size:30px;"
        }

        background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))
      }

      title = content.title

      currentContent = Some(content)
    }

    dialogPane().getStylesheets.add(getClass.getResource("/it/unibo/pps/ese/view/sections/configuration/red-border.css").toExternalForm)

    onCloseRequest = _ =>
      if (currentContent.isDefined && currentContent.get.depth > 1)
        cleanPendingAnimals()

    override def show(): Unit = showAndWait()

    override def closeDialog(): Unit = {
      this.close()
    }

    override def addToPendingAnimals(id: String): Unit = pendingAnimals += id

    override def deleteFromPendingAnimals(id: String): Unit = pendingAnimals -= id

    override def cleanPendingAnimals(): Unit = {
        pendingAnimals.foreach(animal => EntitiesInfo.instance().deleteAnimal(animal))
        pendingAnimals = ListBuffer.empty
      }
  }


}



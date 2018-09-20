package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.view.{MainComponent, SetupViewBridge}
import scalafx.scene.layout._
import scalafx.stage.Window
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{Button, ButtonType, Label}
import scalafx.scene.paint.Color

trait MainDialog {
  def title: StringProperty
  def title_=(title: String)
  def headerText: StringProperty
  def headerText_=(headerText: String)
  def setContent(content: DialogPane)
  def show()
  def window: Window
  def closeDialog()
}

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
            previousPlantsCount: Map[String, Int] = Map.empty): MainDialog =
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
                       previousPlantsCount: Map[String, Int] = Map.empty) extends AbstractDialog[Unit](window, None) with MainDialog {

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
    }

    dialogPane().getStylesheets.add(getClass.getResource("/it/unibo/pps/ese/view/configuration/red-border.css").toExternalForm)

    override def show(): Unit = showAndWait()

    override def closeDialog(): Unit = this.close()
  }


}



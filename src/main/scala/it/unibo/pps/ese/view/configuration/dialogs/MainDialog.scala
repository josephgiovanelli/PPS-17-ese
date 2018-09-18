package it.unibo.pps.ese.view.configuration.dialogs

import it.unibo.pps.ese.view.MainComponent
import scalafx.scene.layout.Pane
import scalafx.stage.Window
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.scene.Node
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{Button, ButtonType}

trait MainDialog {
  def title: StringProperty
  def title_=(title: String)
  def headerText: StringProperty
  def headerText_=(headerText: String)
  def setContent(content: Pane)
  def show()
}

object MainDialog {

  def apply(window: Window,
            mainComponent: MainComponent,
            setUp: Boolean,
            previousAnimalsCount: Map[String, Int] = Map.empty,
            previousPlantsCount: Map[String, Int] = Map.empty): MainDialog =
    new MainDialogImpl(window, mainComponent, setUp, previousAnimalsCount, previousPlantsCount)

  class MainDialogImpl(window: Window,
                       mainComponent: MainComponent,
                       setUp: Boolean,
                       previousAnimalsCount: Map[String, Int],
                       previousPlantsCount: Map[String, Int]) extends AbstractDialog[Unit](window, None) with MainDialog {

    dialogPane().content = ConfigurationPane(this, None, mainComponent, setUp, previousAnimalsCount, previousPlantsCount)

    def setContent(content: Pane): Unit = dialogPane().content = content

    dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)

    override def show(): Unit = showAndWait()
  }


}



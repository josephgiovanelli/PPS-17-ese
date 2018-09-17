package it.unibo.pps.ese.view.start

import it.unibo.pps.ese.controller.loader.data.builder.exception.CompleteBuildException
import scalafx.scene.control.{Alert, ScrollPane}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.text.{Text, TextFlow}
import scalafx.stage.Window

trait NoCompleteSimulationAlert extends Alert

object NoCompleteSimulationAlert {

  def apply(owner: Window, completeBuildException: CompleteBuildException): NoCompleteSimulationAlert = new NoCompleteSimulationAlertImpl(owner, completeBuildException)

  private class NoCompleteSimulationAlertImpl(owner: Window, completeBuildException: CompleteBuildException) extends Alert(AlertType.Error) with NoCompleteSimulationAlert {
    initOwner(owner)
    title = "Problem with simulation loading!"
    headerText = "Given simulation is not complete"
    dialogPane().setExpandableContent(new ScrollPane() {
      content = new TextFlow()() {
        children.add(new Text(completeBuildException.toString))
      }
    })
  }
}
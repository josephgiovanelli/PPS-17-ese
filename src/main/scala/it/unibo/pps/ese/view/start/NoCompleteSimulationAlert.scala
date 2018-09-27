package it.unibo.pps.ese.view.start

import it.unibo.pps.ese.controller.simulation.loader.data.builder.exception.CompleteBuildException
import scalafx.scene.control.{Alert, ScrollPane}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.text.{Text, TextFlow}
import scalafx.stage.Window
import scalafx.Includes._

/** Error alert for unexpected partial simulation data when a complete simulation data is required*/
trait NoCompleteSimulationAlert extends Alert

/** Factory object for [[it.unibo.pps.ese.view.start.NoCompleteSimulationAlert]]*/
object NoCompleteSimulationAlert {

  /**
    *
    * @param owner Owner window
    * @param completeBuildException Occurred exception
    * @return New [[it.unibo.pps.ese.view.start.NoCompleteSimulationAlert]]
    */
  def apply(owner: Window, completeBuildException: CompleteBuildException): NoCompleteSimulationAlert = new NoCompleteSimulationAlertImpl(owner, completeBuildException)

  private class NoCompleteSimulationAlertImpl(owner: Window, completeBuildException: CompleteBuildException) extends Alert(AlertType.Error) with NoCompleteSimulationAlert {
    initOwner(owner)
    title = "Problem with simulation loading!"
    headerText = "Given simulation is not complete"
    dialogPane().expandableContent = new ScrollPane() {
      content = new TextFlow() {
        children.add(new Text(completeBuildException.toString))
      }
    }
  }
}
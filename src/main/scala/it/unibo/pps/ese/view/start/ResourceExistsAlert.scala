package it.unibo.pps.ese.view.start

import it.unibo.pps.ese.controller.util.io.ExistingResource
import scalafx.scene.control.{Alert, ButtonType}
import scalafx.scene.control.Alert.AlertType
import scalafx.stage.Window

trait ResourceExistsAlert extends Alert {

}

object ResourceExistsAlert {
  private class ResourceExistsAlertImpl(owner: Window, existingResource: ExistingResource) extends Alert(AlertType.Confirmation) with ResourceExistsAlert {
    initOwner(owner)
    title = "Problem with saving files"
    headerText = "One resource that have to been saved already exists: " + existingResource
    contentText = "Do you want to override it?"

    buttonTypes = Seq(
      Buttons.Override, Buttons.OverrideAll, ButtonType.Cancel)
  }

  object Buttons {
    val Override = new ButtonType("Yes")
    val OverrideAll = new ButtonType("Yes for all existing")
  }
}
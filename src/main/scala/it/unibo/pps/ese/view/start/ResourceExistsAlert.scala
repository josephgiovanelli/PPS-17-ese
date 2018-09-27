package it.unibo.pps.ese.view.start

import it.unibo.pps.ese.controller.simulation.loader.io.ExistingResource
import scalafx.scene.control.{Alert, ButtonType}
import scalafx.scene.control.Alert.AlertType
import scalafx.stage.Window

/** Alert to notify resource existence during file saving. The result can be one of buttons in [[it.unibo.pps.ese.view.start.ResourceExistsAlert.Buttons]]*/
trait ResourceExistsAlert extends Alert

/** Factory object for [[it.unibo.pps.ese.view.start.ResourceExistsAlert]]*/
object ResourceExistsAlert {

  /**
    * @param owner Owner window
    * @param existingResource Existing resource
    * @return New [[it.unibo.pps.ese.view.start.ResourceExistsAlert]]
    */
  def apply(owner: Window, existingResource: ExistingResource): ResourceExistsAlert = new ResourceExistsAlertImpl(owner, existingResource)

  private class ResourceExistsAlertImpl(owner: Window, existingResource: ExistingResource) extends Alert(AlertType.Confirmation) with ResourceExistsAlert {
    initOwner(owner)
    title = "Problem with saving files"
    headerText = "One resource that have to been saved already exists: " + existingResource
    contentText = "Do you want to override it?"

    buttonTypes = Seq(
      Buttons.Override, Buttons.OverrideAll, ButtonType.Cancel)
  }

  /** Contains buttons of [[it.unibo.pps.ese.view.start.ResourceExistsAlert]]*/
  object Buttons {
    /** User wants to override resource*/
    val Override = new ButtonType("Yes")
    /** User wants to override resource and all other possible existing resources*/
    val OverrideAll = new ButtonType("Yes for all existing")
  }
}
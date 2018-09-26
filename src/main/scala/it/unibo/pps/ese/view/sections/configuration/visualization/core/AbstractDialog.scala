package it.unibo.pps.ese.view.sections.configuration.visualization.core

import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{ButtonType, Dialog}
import scalafx.scene.layout.{Background, BackgroundFill, CornerRadii}
import scalafx.scene.paint.Color
import scalafx.stage.Window

/**
  * An abstract dialog that specify a common features
  *
  * @param window the window of the parent
  * @param key if present is useful to restart information
  * @tparam A the type of the return
  */
abstract class AbstractDialog[A](window: Window, key: Option[String] = None) extends Dialog[A] {

  initOwner(window)

  val cancelCloseButtonType: ButtonType = new ButtonType("Exit", ButtonData.CancelClose)
  dialogPane().buttonTypes = Seq(cancelCloseButtonType)
  val cancelCloseButton: Node = dialogPane().lookupButton(cancelCloseButtonType)
  cancelCloseButton.visible = false

  dialogPane().background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))


}

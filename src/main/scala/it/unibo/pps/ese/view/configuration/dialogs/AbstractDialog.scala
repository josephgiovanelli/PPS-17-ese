package it.unibo.pps.ese.view.configuration.dialogs

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.{Background, BackgroundFill, CornerRadii, GridPane}
import scalafx.scene.paint.Color
import scalafx.stage.Window

abstract class AbstractDialog[A](window: Window, key: Option[String] = None) extends Dialog[A] {

  val ROW_HEIGHT = 26
  val MIN_ELEM = 3

  initOwner(window)

  val cancelCloseButtonType: ButtonType = new ButtonType("Exit", ButtonData.CancelClose)
  dialogPane().buttonTypes = Seq(cancelCloseButtonType)
  val cancelCloseButton: Node = dialogPane().lookupButton(cancelCloseButtonType)
  cancelCloseButton.visible = false

  dialogPane().background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))


}

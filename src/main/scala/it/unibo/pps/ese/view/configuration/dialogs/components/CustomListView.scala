package it.unibo.pps.ese.view.configuration.dialogs.components

import scalafx.geometry.Insets
import scalafx.scene.control.ListView

class CustomListView[A] extends ListView[A] {

  margin = Insets(4, 0, 0, 0)
}

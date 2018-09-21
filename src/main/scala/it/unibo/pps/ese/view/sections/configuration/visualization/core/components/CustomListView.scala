package it.unibo.pps.ese.view.sections.configuration.visualization.core.components

import scalafx.geometry.Insets
import scalafx.scene.control.ListView

class CustomListView[A] extends ListView[A] {

  margin = Insets(4, 0, 0, 0)
}

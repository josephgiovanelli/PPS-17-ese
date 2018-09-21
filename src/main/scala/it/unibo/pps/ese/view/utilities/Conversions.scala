package it.unibo.pps.ese.view.utilities

import it.unibo.pps.ese.view.core.{BodyViewer, HistoryViewer}
import it.unibo.pps.ese.view.main.WorldView

object Conversions {
  implicit def WorldViewToBodyViewer(worldView: WorldView):BodyViewer = worldView.asInstanceOf[BodyViewer]
  implicit def WorldViewToHistoryViewer(worldView: WorldView):HistoryViewer = worldView.asInstanceOf[HistoryViewer]
}

package it.unibo.pps.ese.view

import scalafx.scene.control.ScrollPane

trait HistoryPane extends ScrollPane{

}
object HistoryPane{
  def apply(): HistoryPane = new HistoryPaneImpl()
  private class HistoryPaneImpl() extends HistoryPane {

  }
}

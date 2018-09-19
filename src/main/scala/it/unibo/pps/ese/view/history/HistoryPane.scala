package it.unibo.pps.ese.view.history

import java.lang.Math

import it.unibo.pps.ese.view.utilities.TextUtilities._
import it.unibo.pps.ese.view.history.LogConversions._
import javafx.collections.{FXCollections, ObservableList}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Orientation
import scalafx.scene.control.ListView
import scalafx.scene.layout.{BorderPane, HBox, Pane}

import scala.collection.JavaConverters._
trait HistoryPane extends Pane{
  def updateHistoryLog(newLog:HistoryLog):Unit
}
object HistoryPane{
  def apply(): HistoryPane = new HistoryPaneImpl()
  private class HistoryPaneImpl() extends HistoryPane {
    val historyAggregator = HistoryAggregator()
    val mainPane = new BorderPane()
    val title:HBox = "The World History".toHBox
    title.prefWidth <== width
    mainPane.top = title
    children += mainPane
    mainPane.translateY = 10
    val logsList:ObservableList[Log] = FXCollections.observableArrayList()
    val logs:ListView[Log] = new ListView[Log](logsList)
    logs.orientation = Orientation.Vertical
    logs.prefWidth <== width
    logs.prefHeight <== (height-title.height-10)
    logs.orientation = Orientation.Vertical
    mainPane.center = logs
    logs.cellFactory = {
      p=>{
        new LogListViewCell()
      }
    }
    override def updateHistoryLog(newLog: HistoryLog): Unit = {
      Platform.runLater{
        ()->{
          logsList.addAll(newLog.allLogsWithAggregation(historyAggregator).asJava)
          logs.scrollTo(logsList.size() -1)
        }
      }
    }
  }
}

package it.unibo.pps.ese.view
import LogConversions._
import scalafx.scene.control.{ListView, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox, Pane, VBox}
import it.unibo.pps.ese.view.speciesdetails.TextUtilities._
import javafx.collections.{FXCollections, ObservableList}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Orientation

trait HistoryPane extends Pane{
  def updateHistoryLog(newLog:HistoryLog):Unit
}
object HistoryPane{
  def apply(): HistoryPane = new HistoryPaneImpl()
  private class HistoryPaneImpl() extends HistoryPane {
    val mainPane = new BorderPane()
    val title:HBox = "The World History".toHBox
    title.prefWidth <== width
    mainPane.top = title
    children += mainPane
    mainPane.translateY = 10
    val logsList:ObservableList[Log] = FXCollections.observableArrayList()
    val logs:ListView[Log] = new ListView[Log](logsList)
    logs.orientation = Orientation.Vertical
    logs.prefWidth = width.value
    logs.minHeight = 760
    logs.orientation = Orientation.Vertical
    mainPane.center = logs
    logs.cellFactory = {
      p=>{
        new LogListViewCell()
      }
    }
    override def updateHistoryLog(newLog: HistoryLog): Unit = {
      println(newLog)
      Platform.runLater{
        ()->{
          import scala.collection.JavaConverters._
          logsList.addAll(newLog.allLogs.asJava)
          logs.scrollTo(logsList.size() -1)
        }
      }
    }
  }
}

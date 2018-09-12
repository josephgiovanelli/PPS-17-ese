package it.unibo.pps.ese.view
import LogConversions._
import scalafx.scene.control.{ListView, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import it.unibo.pps.ese.view.speciesdetails.TextUtilities._
import javafx.collections.{FXCollections, ObservableList}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Orientation

trait HistoryPane extends ScrollPane{
  def updateHistoryLog(newLog:HistoryLog):Unit
}
object HistoryPane{
  def apply(): HistoryPane = new HistoryPaneImpl()
  private class HistoryPaneImpl() extends HistoryPane {
    val mainPane = new BorderPane()
    val vbox = new VBox()
    vbox.translateY = 10
    val title:HBox = "The World History".toHBox
    title.prefWidth <== width
    mainPane.top = title
    mainPane.center = vbox
    content = mainPane
    val logsList:ObservableList[Log] = FXCollections.observableArrayList()
    val logs:ListView[Log] = new ListView[Log](logsList)
    logs.orientation = Orientation.Vertical
    logs.prefWidth <== width
    logs.prefHeight <== height
    logs.orientation = Orientation.Vertical
//    vvalue<==logs.height
    logs.cellFactory = {
      p=>{
        new LogListViewCell()
      }
    }
    vbox.spacing = 10
    vbox.children = List(logs)
    override def updateHistoryLog(newLog: HistoryLog): Unit = {
      println(newLog)
      Platform.runLater{
        ()->{
          import scala.collection.JavaConverters._
          logsList.addAll(newLog.allLogs.asJava)
        }
      }
    }
  }
}

package it.unibo.pps.ese.view

import javafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control._
import WorldPrefernces._
import scalafx.scene.layout.BorderPane

object ZoomPreferences {
  val minZoom: Int = 1
  val maxZoom: Int = 8
  val prefZoom: Int = 3
}

private class MainScene(width: Double = 1200, heigth: Double = 800) extends Scene(width, heigth) {

  val menuBar = new MenuBar()
  val fileMenu = new Menu("File")
  val newItem = new MenuItem("New")
  val openItem = new MenuItem("Open")
  val saveItem = new MenuItem("Save")
  val exitItem = new MenuItem("Exit")
  fileMenu.items = List(newItem, openItem, saveItem, new SeparatorMenuItem, exitItem)

  exitItem.onAction = (e: ActionEvent) => {
    sys.exit(0)
  }

  val worldTab = new Tab()
  worldTab.text = "World"
  worldTab.closable = false

  val worldContainerPane = new BorderPane()
  val detailsPane = DetailsPane()
  val worldPane: WorldPane = WorldPane(worldWidth, worldHeigth, detailsPane)
  detailsPane.prefWidth = 300
  detailsPane.prefHeight <== worldContainerPane.height


  worldContainerPane.center = worldPane
  worldContainerPane.right = detailsPane
  worldTab.content = worldContainerPane

  val zoomSlider = new Slider(ZoomPreferences.minZoom, ZoomPreferences.maxZoom, ZoomPreferences.prefZoom)
  zoomSlider.accessibleText = "Zoom"
  worldPane.entitySize <== zoomSlider.value

  val statisticsTab = new Tab()
  statisticsTab.text = "Statistics"
  statisticsTab.closable = false

  val simulationPane = new TabPane()
  simulationPane.tabs = List(worldTab, statisticsTab)

  val mainPane = new BorderPane()
  mainPane.top = zoomSlider
  mainPane.center = simulationPane

  val contentPane = new BorderPane()
  contentPane.top = menuBar
  contentPane.center = mainPane

  root = contentPane
}

package it.unibo.pps.ese.view

import scalafx.Includes._
import javafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control._
import WorldPrefernces._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.bodyViewer.BodyPane
import it.unibo.pps.ese.view.speciesdetails.GenomeDetailsPane
import it.unibo.pps.ese.view.statistics.StatisticsDetailsPane
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.layout.BorderPane

object ZoomPreferences {
  val minZoom: Int = 1
  val maxZoom: Int = 8
  val prefZoom: Int = 3
}


private class MainScene(geneticsSimulator: GeneticsSimulator, mainComponent: MainComponent, width: Double = 1400, height: Double = 900) extends Scene(width, height) with WorldView {

  val generationTextLabel: String = "Generation: "

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

  val bodyPane = BodyPane()
  val genomePane = GenomeDetailsPane(None)

  val worldContainerPane = new SplitPane()
  val historyPane = HistoryPane()
  val detailsPane = DetailsPane(mainComponent)
  val worldPane: WorldPane = WorldPane(geneticsSimulator,mainComponent, detailsPane,genomePane, worldWidth, worldHeigth)
  detailsPane.prefHeight <== worldContainerPane.height

  worldContainerPane.orientation = Orientation.Horizontal
  worldContainerPane.items ++= List(historyPane,worldPane, detailsPane)
  worldContainerPane.setDividerPositions(0.3,0.8,0.15)
  worldTab.content = worldContainerPane

  val zoomSlider = new Slider(ZoomPreferences.minZoom, ZoomPreferences.maxZoom, ZoomPreferences.prefZoom)
  zoomSlider.accessibleText = "Zoom"
  worldPane.entitySize <== zoomSlider.value

  val zoomLabel = new Label("Zoom")

  val zoomPane = new BorderPane()
  zoomPane.left = zoomLabel
  zoomPane.center = zoomSlider
  zoomPane.padding = Insets(10, 50, 10, 50)

  val generationLabel = new Label(generationTextLabel + 0)
  val generationPane = new BorderPane()
  generationPane.center = generationLabel

  val topPane = new BorderPane()
  topPane.bottom = generationPane
  topPane.center = zoomPane

  val statisticsTab = new Tab()
  statisticsTab.text = "Statistics"
  statisticsTab.closable = false
  statisticsTab.content = StatisticsDetailsPane()

  val genomeTab = new Tab()
  genomeTab.text = "Genome"
  genomeTab.closable = false
  genomeTab.content = genomePane

  val bodyTab = new Tab()
  bodyTab.text = "Body Parts"
  bodyTab.closable = false
  bodyTab.content = bodyPane

  val simulationPane = new TabPane()
  simulationPane.tabs = List(worldTab, statisticsTab,genomeTab,bodyTab)
  val mainPane = new BorderPane()
  mainPane.top = topPane
  mainPane.center = simulationPane

  val contentPane = new BorderPane()
  contentPane.top = menuBar
  contentPane.center = mainPane

  root = contentPane

  override def updateWorld(generation: Int, world: List[Entity]): Unit = {
    generationLabel.text = generationTextLabel + generation
    worldPane.updateWorld(generation, world)
  }
}

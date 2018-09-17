package it.unibo.pps.ese.view

import scalafx.Includes._
import javafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control._
import WorldPrefernces._
import it.unibo.pps.ese.genericworld.model.EntityState
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.configuration.dialogs.ConfigurationDialog
import it.unibo.pps.ese.view.bodyViewer.{AnimalInternalStatus, BodyPane}
import it.unibo.pps.ese.view.configuration.dialogs.ConfirmDialog
import it.unibo.pps.ese.view.filters.FiltersPane
import it.unibo.pps.ese.view.history.{HistoryLog, HistoryPane}
import it.unibo.pps.ese.view.speciesdetails.GenomeDetailsPane
import it.unibo.pps.ese.view.statistics.StatisticsDetailsPane
import javafx.application.Platform
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.layout.BorderPane

import scala.concurrent.ExecutionContext

object ZoomPreferences {
  val minZoom: Int = 1
  val maxZoom: Int = 8
  val prefZoom: Int = 3
}


private class MainScene(geneticsSimulator: GeneticsSimulator, mainComponent: MainComponent, width: Double = 1200, height: Double = 800)
                       (implicit executionContext: ExecutionContext) extends Scene(width, height) with WorldView with HistoryViewer with BodyViewer {


  val generationTextLabel: String = "Generation: "
  val currentWindow: scalafx.stage.Window = this.window()

  val menuBar = new MenuBar()
  val fileMenu = new Menu("Simulation")
  val editMenu = new Menu("Edit")
  val addEntitiesItem = new MenuItem("Add Entities")
  val addSpeciesItem = new MenuItem("Add Species")
  val pauseItem = new MenuItem("Pause")
  val playItem = new MenuItem("Restart")

  fileMenu.items = List(pauseItem, playItem)
  editMenu.items = List(addEntitiesItem, addSpeciesItem)
  menuBar.menus = List(fileMenu,editMenu)

  playItem.disable = true

  pauseItem.onAction = (e: ActionEvent) => {
    pauseItem.disable = true
    playItem.disable = false
    mainComponent.pause()
  }

  playItem.onAction = (e: ActionEvent) => {
    playItem.disable = true
    pauseItem.disable = false
    mainComponent.play()
  }

  addEntitiesItem.onAction = (e: ActionEvent) => {
    ConfirmDialog(currentWindow, mainComponent, setUp = false).showAndWait()
  }

  addSpeciesItem.onAction = (e: ActionEvent) => {
    ConfigurationDialog(currentWindow, mainComponent, setUp = false).showAndWait()
  }

  val worldTab = new Tab()
  worldTab.text = "World"
  worldTab.closable = false

  val bodyPane = BodyPane()
  val genomePane = GenomeDetailsPane(None)

  val worldContainerPane = new SplitPane()
  val historyPane:HistoryPane = HistoryPane()
  val detailsPane = DetailsPane(mainComponent)
  val worldPane: WorldPane = WorldPane(geneticsSimulator, mainComponent, this, detailsPane,genomePane, worldWidth, worldHeigth)
  detailsPane.prefHeight <== worldContainerPane.height

  worldContainerPane.orientation = Orientation.Horizontal
  worldContainerPane.items ++= List(historyPane,worldPane, detailsPane)
  worldContainerPane.setDividerPositions(0.22,0.78)
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

  val filtersTab = new Tab()
  filtersTab.text = "Filters"
  filtersTab.closable = false
  filtersTab.content = FiltersPane(worldPane, geneticsSimulator)

  val statisticsPane = StatisticsDetailsPane(mainComponent)

  val statisticsTab = new Tab()
  statisticsTab.text = "Statistics"
  statisticsTab.closable = false
  statisticsTab.content = statisticsPane
  statisticsTab.onSelectionChanged = _ =>
    if (statisticsTab.isSelected) {
      mainComponent historicalData() foreach (y => Platform.runLater {() => statisticsPane initializeCharts y })
      mainComponent simulationEras() foreach(y => Platform.runLater {() => statisticsPane populateEraDropdown y })
    }

  val genomeTab = new Tab()
  genomeTab.text = "Genome"
  genomeTab.closable = false
  genomeTab.content = genomePane

  val bodyTab = new Tab()
  bodyTab.text = "Body Parts"
  bodyTab.closable = false
  bodyTab.content = bodyPane

  val simulationPane = new TabPane()
  simulationPane.tabs = List(worldTab, filtersTab, statisticsTab,genomeTab,bodyTab)
  val mainPane = new BorderPane()
  mainPane.top = topPane
  mainPane.center = simulationPane

  val contentPane = new BorderPane()
  contentPane.top = menuBar
  contentPane.center = mainPane

  root = contentPane

  override def updateWorld(generation: Int, world: Seq[EntityState]): Unit = {
    generationLabel.text = generationTextLabel + generation
    worldPane.updateWorld(generation, world)
  }

  override def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus): Unit = {
    bodyPane.updateAnimalInternalStatus(animalInternalStatus)
  }

  override def updateHistoryLog(newLog: HistoryLog): Unit = {
    historyPane.updateHistoryLog(newLog)
  }

  override def clearStatus(): Unit = bodyPane.clearStatus()
}

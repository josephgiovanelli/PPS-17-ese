package it.unibo.pps.ese.view.sections.statistics

import javafx.application.Platform

import it.unibo.pps.ese.view.core.MainComponent

import scalafx.collections.ObservableBuffer
import scalafx.geometry._
import scalafx.scene.chart._
import scalafx.scene.control.{Button, ComboBox, ScrollPane}
import scalafx.scene.layout._
import scalafx.Includes._
import scalafx.scene.paint.Color
import scala.concurrent.ExecutionContext

case class ChartsData(populationTrend: Seq[(String, Seq[(Long, Long)])],
                      populationDistribution: Seq[(String, Long)],
                      births: Seq[(String, Seq[(Long, Long)])],
                      mutations: Seq[(String, Seq[(Long, Long)])])

sealed trait StatisticsDetailsPane extends BorderPane {
  def initializeCharts(chartsData: ChartsData)
  def populateEraDropdown(era: Seq[Long])
}
object StatisticsDetailsPane {

  def apply(mainComponent: MainComponent)
           (implicit executionContext: ExecutionContext):StatisticsDetailsPane =
    new WebBasedStatisticsDetailsPane(mainComponent)

  private[this] class WebBasedStatisticsDetailsPane(mainComponent: MainComponent)
            (implicit executionContext: ExecutionContext) extends StatisticsDetailsPane {

    prefWidth <== 1000
    prefHeight <== 800
    background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0),
      CornerRadii.Empty, Insets.Empty)))

    val eraCombo: ComboBox[String] = new ComboBox[String] {
      maxWidth = 200
      promptText = "Choose a era..."
      onAction = _ => populateEntityDropdown(eraCombo.value.value)
    }

    val entityCombo: ComboBox[String] = new ComboBox[String] {
      maxWidth = 200
      promptText = "Choose an entity..."
      onAction = _ => replayButton setDisable (entityCombo.value.value == null)
    }

    val replayButton: Button = new Button {
      text = "Replay"
      prefWidth <== 125
      disable = true
      onMouseClicked = _ => new ReplayStage(entityCombo.value.value, mainComponent replay) showAndWait()
    }

    val refreshButton: Button = new Button("Refresh") {
      onAction = _ => {
        mainComponent historicalData() foreach (y => Platform.runLater {() => initializeCharts(y) })
        mainComponent simulationEras() foreach(y => Platform.runLater {() => {
          entityCombo.getItems.clear()
          replayButton setDisable true
          populateEraDropdown(y)
        }})
      }
    }

    val controlReplayHBox: HBox = new HBox {
      spacing = 10
      margin = Insets(10, 0, 10, 0)
      children addAll (refreshButton, eraCombo, entityCombo, replayButton)
      alignmentInParent = Pos.Center
      alignment = Pos.Center
    }

    controlReplayHBox.prefWidth <== width

    val borderPane: BorderPane = new BorderPane {
      alignmentInParent = Pos.Center
      center = controlReplayHBox
    }

    val barChart: LineChart[Number, Number] = new LineChart(NumberAxis("Era"), NumberAxis("Births")) {
      title = "Births Chart"
      legendSide = Side.Right
      createSymbols = false
    }

    val areaChart: AreaChart[Number, Number] = new AreaChart(NumberAxis("Era"), NumberAxis("Mutations")) {
      title = "Mutations"
      legendSide = Side.Right
      createSymbols = false
    }

    val pieChart: PieChart = new PieChart {
      title = "Alive Entities"
      clockwise = false
    }

    val categoryLine: LineChart[Number, Number] = new LineChart(NumberAxis("Era"), NumberAxis("Population")) {
      title = "Population Trend"
      legendSide = Side.Right
      createSymbols = false
    }

    val flowPane: FlowPane = new FlowPane() {
      orientation = Orientation.Vertical
      padding = Insets(5, 0, 5, 0)
      hgap = 4
      vgap = 4
      prefWrapLength = 1000
      children = List(categoryLine, barChart, pieChart, areaChart)
    }

    flowPane.translateX = 100

    val scrollPane: ScrollPane = new ScrollPane {
      content = flowPane
      hbarPolicy = ScrollPane.ScrollBarPolicy.Never
    }

    scrollPane.prefWidth <== width
    scrollPane.prefHeight <== height

    top = borderPane
    center = scrollPane

    override def initializeCharts(chartsData: ChartsData): Unit = {

      def populationChart(series: Seq[(String, Seq[(Long, Long)])]): Unit = {

        def xySeriesCategory(name: String, data: Seq[(Long, Long)]) = {
          XYChart.Series[Number, Number](
            name,
            ObservableBuffer(data.map {case (x, y) => XYChart.Data[Number, Number](x, y)})
          )
        }

        categoryLine.getData.clear()
        series.map(x => xySeriesCategory(x._1, x._2)).foreach(x => categoryLine.getData.add(x))
      }

      def populationDistributionChart(series: Seq[(String, Long)]): Unit = {
        pieChart.getData.clear()
        ObservableBuffer(series.map {case (x, y) => PieChart.Data(x, y)}).foreach(x => pieChart.getData.add(x))
      }

      def birthChart(series: Seq[(String, Seq[(Long, Long)])]): Unit = {

        def xySeriesCategory(name: String, data: Seq[(Long, Long)]) = {
          XYChart.Series[Number, Number](
            name,
            ObservableBuffer(data.map {case (x, y) => XYChart.Data[Number, Number](x, y)})
          )
        }

        barChart.getData.clear()
        series.map(x => xySeriesCategory(x._1, x._2)).foreach(y => barChart.getData.add(y))
      }

      def mutationsChart(series: Seq[(String, Seq[(Long, Long)])]): Unit = {

        def xySeriesArea(name: String, data: Seq[(Long, Long)]) =
          XYChart.Series[Number, Number](
            name,
            ObservableBuffer(data.map {case (x, y) => XYChart.Data[Number, Number](x, y)})
          )

        areaChart.getData.clear()
        series.map(x => xySeriesArea(x._1, x._2)).foreach(y => areaChart.getData.add(y))
      }

      populationChart(chartsData populationTrend)
      populationDistributionChart(chartsData populationDistribution)
      birthChart(chartsData births)
      mutationsChart(chartsData mutations)
    }

    def populateEraDropdown(era: Seq[Long]): Unit = {
      eraCombo.getItems.clear()
      era.foreach(y => eraCombo.getItems.add(y.toString))
    }

    def populateEntityDropdown(era: String): Unit = {
      if (era != null) {
        entityCombo.getItems.clear()
        mainComponent.entitiesInEra(era.toLong) foreach (y => Platform.runLater {() => {
          y foreach(x => entityCombo.getItems.add(x))
        }})
      }
    }
  }
}

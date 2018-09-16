package it.unibo.pps.ese.view.statistics

import it.unibo.pps.ese.view.MainComponent
import javafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{HPos, Insets, Orientation, Side}
import scalafx.scene.Group
import scalafx.scene.chart._
import scalafx.scene.control.{Button, ComboBox, ScrollPane}
import scalafx.scene.layout._

import scala.concurrent.ExecutionContext

case class ChartsData(populationTrend: Seq[(String, Seq[(Long, Long)])],
                      populationDistribution: Seq[(String, Long)],
                      births: Seq[(String, Seq[(Long, Long)])],
                      mutations: Seq[(String, Seq[(Long, Long)])])

sealed trait StatisticsDetailsPane extends ScrollPane {
  def initializeCharts(chartsData: ChartsData)
  def populateEraDropdown(era: Seq[Long])
}
object StatisticsDetailsPane {

  def apply(mainComponent: MainComponent)
           (implicit executionContext: ExecutionContext):StatisticsDetailsPane = new WebBasedStatisticsDetailsPane(mainComponent)

  private[this] class WebBasedStatisticsDetailsPane(mainComponent: MainComponent)
            (implicit executionContext: ExecutionContext) extends StatisticsDetailsPane {

    fitToWidth = true

    val eraCombo: ComboBox[String] = new ComboBox[String] {
      maxWidth = 200
      promptText = "Choose a era..."
      onAction = _ => populateEntityDropdown(eraCombo.value.value)
    }

    val replayButton: Button = new Button("Replay") {
      prefWidth = 125
      disable = true
      onMouseClicked = _ => {
        val dialogStage = new ReplayStage(entityCombo.value.value, mainComponent replay)
        dialogStage.showAndWait()
      }
    }

    val entityCombo: ComboBox[String] = new ComboBox[String] {
      maxWidth = 200
      promptText = "Choose an entity..."
      onAction = _ => replayButton setDisable (entityCombo.value.value == null)
    }

    val borderPane: BorderPane = new BorderPane {
      center = new HBox {
        //padding = Insets(10, 400, 10, 400)
        children = List(eraCombo, entityCombo, replayButton)
      }
      fitToWidth = true
    }

    GridPane.setConstraints(borderPane, 0, 0)
    GridPane.setHalignment(borderPane, HPos.Center)

    val SCALE_DELTA = 1.1

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

    val root = new Group()
    content = root

    val flowPane: FlowPane = new FlowPane() {
      orientation = Orientation.Vertical
      padding = Insets(5, 0, 5, 0)
      hgap = 4
      vgap = 4
      prefWrapLength = 1000
      fitToWidth = true
    }

    (flowPane children) addAll (categoryLine, barChart, pieChart, areaChart)

//    val vBox: BorderPane = new BorderPane() {
//      top = borderPane
//      center = flowPane
//      prefWidth = 800
//    }
    GridPane.setConstraints(flowPane, 0, 1)
    GridPane.setHalignment(flowPane, HPos.Center)

    val grid: GridPane = new GridPane {
      children = List(borderPane, flowPane)
    }

    (root children) add grid

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

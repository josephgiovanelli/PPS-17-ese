package it.unibo.pps.ese.view.statistics

import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Orientation, Side}
import scalafx.scene.Group
import scalafx.scene.chart._
import scalafx.scene.control.ScrollPane
import scalafx.scene.layout._

case class ChartsData(populationTrend: Seq[(String, Seq[(String, Long)])],
                      populationDistribution: Seq[(String, Long)],
                      births: Seq[(String, Seq[(String, Long)])],
                      mutations: Seq[(String, Seq[(String, Long)])])

sealed trait StatisticsDetailsPane extends ScrollPane {
  def initializeCharts(chartsData: ChartsData)
}
object StatisticsDetailsPane {

  def apply():StatisticsDetailsPane = new WebBasedStatisticsDetailsPane()

  private[this] class WebBasedStatisticsDetailsPane() extends StatisticsDetailsPane {

    prefWidth = 800
    prefHeight = 600

    val barChart: BarChart[String, Number] = new BarChart(CategoryAxis("Era"), NumberAxis("Births")) {
      title = "Births Chart"
    }

    val areaChart: AreaChart[String, Number] = new AreaChart(CategoryAxis("Era"), NumberAxis("Mutations")) {
      title = "Mutations"
      legendSide = Side.Right
    }

    val pieChart: PieChart = new PieChart {
      title = "Alive Entities"
      clockwise = false
    }

    val categoryLine: LineChart[String, Number] = new LineChart(CategoryAxis("Era"), NumberAxis("Population")) {
      title = "Population Trend"
      legendSide = Side.Right
    }

    val root = new Group()
    content = root

    val flowPane: FlowPane = new FlowPane() {
      orientation = Orientation.Vertical
      padding = Insets(5, 0, 5, 0)
      hgap = 4
      vgap = 4
      prefWrapLength = 1000
    }

    (flowPane children) addAll (categoryLine, barChart, pieChart, areaChart)

    (root children) add flowPane

    override def initializeCharts(chartsData: ChartsData): Unit = {

      def populationChart(series: Seq[(String, Seq[(String, Long)])]): Unit = {

        def xySeriesCategory(name: String, data: Seq[(String, Long)]) = {
          XYChart.Series[String, Number](
            name,
            ObservableBuffer(data.map {case (x, y) => XYChart.Data[String, Number](x, y)})
          )
        }

        series.map(x => xySeriesCategory(x._1, x._2)).foreach(x => categoryLine.getData.add(x))
      }

      def populationDistributionChart(series: Seq[(String, Long)]): Unit = {
        pieChart.data = ObservableBuffer(series.map {case (x, y) => PieChart.Data(x, y)})
      }

      def birthChart(series: Seq[(String, Seq[(String, Long)])]): Unit = {

        def xySeriesBar(name: String, data: Seq[(String, Long)]) = {
          XYChart.Series[String, Number](
            name,
            ObservableBuffer(data.map {case (x, y) => XYChart.Data[String, Number](x, y)})
          )
        }

        series.map(x => xySeriesBar(x._1, x._2)).foreach(y => barChart.getData.add(y))
      }

      def mutationsChart(series: Seq[(String, Seq[(String, Long)])]): Unit = {

        def xySeriesArea(name: String, data: Seq[(String, Long)]) =
          XYChart.Series[String, Number](
            name,
            ObservableBuffer(data.map {case (x, y) => XYChart.Data[String, Number](x, y)})
          )

        series.map(x => xySeriesArea(x._1, x._2)).foreach(y => areaChart.getData.add(y))
      }

      populationChart(chartsData populationTrend)
      populationDistributionChart(chartsData populationDistribution)
      birthChart(chartsData births)
      mutationsChart(chartsData mutations)
    }
  }
}

package it.unibo.pps.ese.view.statistics

import it.unibo.pps.ese.view.speciesdetails.Materials.whiteMaterial
import it.unibo.pps.ese.view.speciesdetails.{GeneCouple, GeneCoupleXForm, GeneDetailsSubScene, GenomeDetailsPane, Left, Materials, Right, Xform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Group
import scalafx.scene.control.{Slider, TextField}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{BorderPane, Pane, Priority}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Cylinder, Sphere}
import scalafx.scene.text.TextAlignment
import scalafx.scene.transform.Rotate
import scalafx.scene.web.{WebEvent, WebView}

sealed trait StatisticsDetailsPane extends Pane {

}
object StatisticsDetailsPane {

  def apply():StatisticsDetailsPane = new WebBasedStatisticsDetailsPane()

  private[this] class WebBasedStatisticsDetailsPane() extends StatisticsDetailsPane {

    prefWidth = 1000
    prefHeight = 800

    val browser = new WebView {
      hgrow = Priority.Always
      vgrow = Priority.Always
    }

    import scalax.chart.api._

    val data = for (i <- 1 to 5) yield (i,i)
    val chart = XYLineChart(data)

    //chart show()

    val engine = browser.engine
    engine loadContent "<h1>Prova</h1>"
    //engine.load("http://www.scalafx.org/")

    val txfUrl = new TextField {
      text = engine.location.value
      hgrow = Priority.Always
      vgrow = Priority.Never
    }

    val root = new Group()
    children add root

    val content = new BorderPane {
      hgrow = Priority.Always
      vgrow = Priority.Always
      top = txfUrl
      center = browser
    }

    (root children) add content

  }

}

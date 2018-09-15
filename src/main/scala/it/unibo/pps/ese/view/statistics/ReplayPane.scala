package it.unibo.pps.ese.view.statistics

import java.awt.MouseInfo

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
import scalafx.scene.control.{Alert, ScrollPane, SplitPane, Tooltip}
import javafx.application.Platform
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.{KeyEvent, MouseEvent, ScrollEvent}
import scalafx.scene.paint.Color
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._
import it.unibo.pps.ese.view.WorldPrefernces.{worldHeigth, worldWidth}
import it.unibo.pps.ese.view._
import scalafx.geometry.Orientation

import scala.util.Random

trait ReplayView {
  def updateWorld(generation: Int, world: List[Entity]): Unit
}

trait ReplayPane extends ScrollPane with ReplayView {
  var entitySize: IntegerProperty = IntegerProperty(ZoomPreferences.prefZoom)
}

object ReplayPane {
  def apply(mainComponent: MainComponent, width: Int, height: Int): WorldPane =
    new ReplayPaneImpl(mainComponent, width, height)
}

private class ReplayPaneImpl(mainComponent: MainComponent, width: Int, height: Int) extends WorldPane {

  val selectionColor: Color = Color.Gold

  private var worldViewWidth: DoubleProperty = DoubleProperty(500)
  private var worldViewHeight: DoubleProperty = DoubleProperty(500)
  worldViewWidth <== 800
  worldViewHeight <== 500

  private val canvas = new Canvas()
  canvas.width <== worldViewWidth
  canvas.height <== worldViewHeight

  private val graphicsContext: GraphicsContext = canvas.graphicsContext2D

  val worldContainerPane = new SplitPane()
  val detailsPane = new ScrollPane()
  detailsPane.prefHeight <== 500

  worldContainerPane.orientation = Orientation.Horizontal
  worldContainerPane.items ++= List(canvas, detailsPane)
  worldContainerPane.dividerPositions = 0.7

  content = worldContainerPane

  override def updateWorld(generation: Int, world: List[Entity]): Unit = {
    drawWorld(world)
  }

  //disegnare solo l'intorno dell'entità
  private def drawWorld(world: List[Entity]): Unit = {
    Platform.runLater {
      () => {
        graphicsContext.clearRect(0, 0, worldViewWidth(), worldViewHeight())
        world foreach (e => {
          graphicsContext.fill = e.color
          graphicsContext.fillRect(e.position.x * entitySize(), e.position.y * entitySize(), entitySize(), entitySize())
        })
      }
    }
  }
}

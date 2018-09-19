package it.unibo.pps.ese.view.statistics

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
import scalafx.scene.control.{Alert, ScrollPane, SplitPane, Tooltip}
import javafx.application.Platform

import it.unibo.pps.ese.entitywatchers.ResultEra

import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color
import it.unibo.pps.ese.utils.Point

import scalafx.geometry.Orientation

trait ReplayView {
  def render(data: (Long, ResultEra)): Unit
}

trait ReplayPane extends ScrollPane with ReplayView

object ReplayPane {
  def apply(): ReplayPane = new ReplayPaneImpl()
}

private class ReplayPaneImpl() extends ReplayPane {

  private val worldWidth = 500
  private val worldHeight = 500
  private val entitySize = 5

  private var replayViewWidth: DoubleProperty = DoubleProperty(850)
  private var replayViewHeight: DoubleProperty = DoubleProperty(500)

  private val canvas = new Canvas()
  canvas.width <== worldWidth
  canvas.height <== worldHeight

  private val graphicsContext: GraphicsContext = canvas.graphicsContext2D

  val worldContainerPane = new SplitPane()
  val detailsPane = new DetailsReplayPane()
  detailsPane.prefHeight <== replayViewHeight
  detailsPane.prefWidth <== 350

  worldContainerPane.orientation = Orientation.Horizontal
  worldContainerPane.items ++= List(canvas, detailsPane)

  content = worldContainerPane

  override def render(data: (Long, ResultEra)): Unit = drawWorld(data._1, data._2)

  private def drawWorld(era: Long, world: ResultEra): Unit = {

    def drawTarget(color: Color): Unit = {
      graphicsContext.fill = Color.Gold
      val targetPosition = normalizePosition(world.me.dynamicData.position, world.me.dynamicData.position,
        worldWidth, worldHeight)
      graphicsContext.fillRect(targetPosition.x, targetPosition.y, entitySize, entitySize)
    }

    def drawActors(label: String, color: Color): Unit = {
      graphicsContext.fill = color
      world.others.filter(x => x.label == label).flatMap(x => x.entities).foreach(y => {
        val targetPosition = normalizePosition(y _2, world.me.dynamicData.position, worldWidth, worldHeight)
        if (checkBounds(targetPosition, worldWidth, worldHeight))
          graphicsContext.fillRect(targetPosition.x, targetPosition.y, entitySize, entitySize)
      })
    }

    def logActions(): Unit = {
      if (world.actions.contains("eat"))
        world.actions("eat").foreach(x => detailsPane logInteraction("eats", x, era))
      if (world.actions.contains("couple"))
        world.actions("couple").foreach(x => detailsPane logInteraction("mates", x, era))
      if (world.actions.contains("give birth"))
        world.actions("give birth").foreach(x => detailsPane logInteraction("gives birth to", x, era))
    }

    Platform.runLater {
      () => {
        graphicsContext.clearRect(0, 0, 500, 500)
        drawTarget(Color.Gold)
        drawActors("prey", Color.AliceBlue)
        drawActors("partner", Color.Red)
        drawActors("child", Color.Green)
        drawActors("killer", Color.Black)
        detailsPane showDetails world.me
        logActions()
      }
    }
  }

  private def checkBounds(position: Point, width: Int, height: Int): Boolean =
    position.x >= 0 && position.x <= width && position.y >= 0 && position.y <= height

  private def normalizePosition(target: Point, center: Point, width: Int, height: Int): Point = {
    Point(((width / 2) * target.x) / center.x, ((height / 2) * target.y) / center.y)
  }
}

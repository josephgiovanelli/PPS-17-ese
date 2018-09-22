package it.unibo.pps.ese.view.sections.statistics

import scalafx.Includes._
import scalafx.beans.property.DoubleProperty
import scalafx.scene.control.{ScrollPane, SplitPane}
import javafx.application.Platform
import it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers.ResultEra
import it.unibo.pps.ese.model.dataminer.datamodel.AnimalDynamicDataImpl
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color
import it.unibo.pps.ese.utils.Point
import scalafx.geometry.Orientation
import scalafx.scene.paint.Color.valueOf

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
      val actions: Map[String, Seq[String]] = world.me.dynamicData match {
        case impl: AnimalDynamicDataImpl =>
          Map("eat" -> impl.eating, "couple" -> impl.coupling, "give birth" -> impl.givingBirth)
        case _ => Map()
      }
      if (actions.contains("eat"))
        actions("eat").foreach(x => detailsPane logInteraction("eats", x, era))
      if (actions.contains("couple"))
        actions("couple").foreach(x => detailsPane logInteraction("mates", x, era))
      if (actions.contains("give birth"))
        actions("give birth").foreach(x => detailsPane logInteraction("gives birth to", x, era))
    }

    Platform.runLater {
      () => {
        graphicsContext.fill = valueOf("#2c3e50")
        graphicsContext.fillRect(0, 0, worldWidth, worldHeight)
        drawTarget(Color.Gold)
        drawActors("prey", Color.Violet)
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

    def toNaturalNumber(x: Int): Int = if (x <= 0) 1 else x

    Point(((width / 2) * toNaturalNumber(target.x)) / toNaturalNumber(center.x),
      ((height / 2) * toNaturalNumber(target.y)) / toNaturalNumber(center.y))
  }
}

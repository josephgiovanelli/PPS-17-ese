package it.unibo.pps.ese.view

import java.awt.MouseInfo

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
import scalafx.scene.control.{Alert, ScrollPane, Tooltip}
import WorldPrefernces._
import ZoomPreferences._
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.{KeyEvent, MouseEvent, ScrollEvent}
import scalafx.scene.paint.Color
import scalafx.stage.Popup

import scala.util.Random

trait WorldPane extends ScrollPane {
  var entitySize: IntegerProperty = IntegerProperty(ZoomPreferences.prefZoom)

  def update(entities: List[Entity])
}

object WorldPane {
  def apply(width: Int, heigth: Int, detailsPane: DetailsPane): WorldPane = new WorldPaneImpl(width, heigth, detailsPane)
}

class WorldPaneImpl(width: Int, heigth: Int, detailsPane: DetailsPane) extends WorldPane {

  private var currentWorld: Map[Position, Entity] = Map()
  private var currentSelected: Option[(Position, Entity)] = None

  private var worldViewWidth: DoubleProperty = DoubleProperty(worldWidth*entitySize())
  private var worldViewHeigth: DoubleProperty = DoubleProperty(worldHeigth*entitySize())
  worldViewWidth <== entitySize*worldWidth
  worldViewHeigth <== entitySize*worldHeigth

  private val canvas = new Canvas()
  canvas.width <== worldViewWidth
  canvas.height <== worldViewHeigth

  private val graphicsContext: GraphicsContext = canvas.graphicsContext2D

  content = canvas

  val r: Random = Random
  update((0 until 3000)
    .map(x => Entity(
      name = "a"+x,
      color = Color.Green,
      position = Position(r.nextInt(worldWidth), r.nextInt(worldHeigth))))
    .toList)

  entitySize.onChange(update(currentWorld.values.toList))

  tooltip = new Tooltip()
  canvas.onMouseMoved = (e: MouseEvent) => {
//    println(getEntityRealPosition(e.x, e.y) + ", " + getEntityWorldViewPosition(e.x, e.y))
    val pos: Position = getEntityWorldViewPosition(e.x, e.y)
    currentWorld.get(pos) match {
      case Some(entity) =>
        tooltip().text = entity.name
        tooltip().show(this, MouseInfo.getPointerInfo.getLocation.x+10, MouseInfo.getPointerInfo.getLocation.y+10)
      case _ =>
        tooltip().hide()
    }
  }

  canvas.onMouseExited = () => {
    tooltip().hide()
  }

  canvas.onMouseClicked = (e: MouseEvent) => {
    currentSelected match {
      case Some(tuple) =>
        graphicsContext.fill = tuple._2.color
        graphicsContext.fillRect(tuple._1.x, tuple._1.y, entitySize(), entitySize())
      case None =>
    }

    val pos: Position = getEntityWorldViewPosition(e.x, e.y)
    currentWorld.get(pos) match {
      case Some(entity) =>
        currentSelected = Some(pos, entity)
        graphicsContext.fill = Color.Red
        graphicsContext.fillRect(pos.x, pos.y, entitySize(), entitySize())
        detailsPane.showDetails(entity)
      case None =>
        currentSelected = None
        detailsPane.clearDetails()

    }


  }

/*  canvas.onScroll = (e: ScrollEvent) => {
    if(e.controlDown) {
      val value = if(e.deltaY>0) 1 else -1
      entitySize = if (entitySize()+value>=minZoom && entitySize()+value<=maxZoom) IntegerProperty(entitySize()+value)
        else entitySize
      update(currentWorld.values.toList)
    }
  }*/


  override def update(entities: List[Entity]): Unit = {
    currentWorld = Map() ++ entities.map(e => (Position(e.position.x*entitySize(), e.position.y*entitySize()), e))

    graphicsContext.clearRect(0, 0, worldViewWidth(), worldViewHeigth())
    entities foreach(e => {
      graphicsContext.fill = e.color
      graphicsContext.fillRect(e.position.x*entitySize(), e.position.y*entitySize(), entitySize(), entitySize())
    })
    currentSelected match {
      case Some(tuple) =>
        currentSelected = Some(Position(tuple._2.position.x * entitySize(), tuple._2.position.y * entitySize()), tuple._2)
        graphicsContext.fill = Color.Red
        graphicsContext.fillRect(currentSelected.get._1.x, currentSelected.get._1.y, entitySize(), entitySize())
      case None =>
    }
  }

  private def getEntityWorldViewPosition(x: Double, y: Double): Position = {
    val posx = x.toInt - (x.toInt%entitySize())
    val posy = y.toInt - (y.toInt%entitySize())
    Position(posx, posy)
  }

  private def getEntityRealPosition(x: Double, y: Double): Position = {
    val pos: Position = getEntityWorldViewPosition(x, y)
    Position(pos.x/entitySize(), pos.y/entitySize())
  }

}
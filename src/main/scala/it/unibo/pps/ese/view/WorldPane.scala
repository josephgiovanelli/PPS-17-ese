package it.unibo.pps.ese.view

import java.awt.MouseInfo

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
import scalafx.scene.control.{Alert, ScrollPane, Tooltip}
import WorldPrefernces._
import ZoomPreferences._
import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genetics.entities.{AnimalInfo, PlantInfo}
import it.unibo.pps.ese.view.speciesdetails.{GenomeDetailsPane, GenomeStatsUtilities}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Position
import it.unibo.pps.ese.genericworld.controller.EntityDetails
import it.unibo.pps.ese.genericworld.model
import it.unibo.pps.ese.genericworld.model.EntityInfo
import javafx.application.Platform
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.{KeyEvent, MouseEvent, ScrollEvent}
import scalafx.scene.paint.Color
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._
import it.unibo.pps.ese.view.bodyViewer.BodyPane

import scala.util.Random

trait WorldView {
  def updateWorld(generation: Int, world: List[Entity]): Unit
}
trait WorldPane extends ScrollPane with WorldView{
  var entitySize: IntegerProperty = IntegerProperty(ZoomPreferences.prefZoom)
}

object WorldPane {
  def apply(
             geneticsSimulator:GeneticsSimulator,
             mainComponent: MainComponent,
             mainScene: MainScene,
             detailsPane: DetailsPane,
             genomeDetailsPane: GenomeDetailsPane,
             width: Int,
             height: Int
           ): WorldPane =
    new WorldPaneImpl(geneticsSimulator, mainComponent, mainScene,
      detailsPane,genomeDetailsPane, width, height)
}

private class WorldPaneImpl(
                             geneticsSimulator:GeneticsSimulator,
                             mainComponent: MainComponent,
                             mainScene: MainScene,
                             detailsPane: DetailsPane,
                             genomeDetailsPane: GenomeDetailsPane,
                             width: Int,
                             height: Int
                           ) extends WorldPane {

  val selectionColor: Color = Color.Gold

  private var currentWorld: Map[Position, Entity] = Map()
  private var currentSelected: Option[String] = None
  private var currentWatched: Option[String] = None
  private var worldViewWidth: DoubleProperty = DoubleProperty(worldWidth*entitySize())
  private var worldViewHeigth: DoubleProperty = DoubleProperty(worldHeigth*entitySize())
  worldViewWidth <== entitySize*worldWidth
  worldViewHeigth <== entitySize*worldHeigth

  private val canvas = new Canvas()
  canvas.width <== worldViewWidth
  canvas.height <== worldViewHeigth

  private val graphicsContext: GraphicsContext = canvas.graphicsContext2D

  content = canvas

  entitySize.onChange(drawWorld(currentWorld.values.toList))

  tooltip = new Tooltip()
  canvas.onMouseMoved = (e: MouseEvent) => {
    val pos: Position = getEntityViewStartPosition(e.x, e.y)
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
      case Some(id) =>
        val entity: Entity = getEntityById(id).get
        val position: Position = getEntityViewPosition(entity)
        clearSelectedEntity(position, entity)
      case None =>
    }

    val pos: Position = getEntityViewStartPosition(e.x, e.y)
    currentWorld.get(pos) match {
      case Some(entity) =>
        val entityDetails:model.EntityInfo = mainComponent.getEntityDetails(entity.id).get
        entityDetails.baseEntityInfo match {
          case AnimalInfo(_,_,_,_,_,_) =>
            genomeDetailsPane.setGenomeStats(GenomeStatsUtilities.buildGenomeStats(
              geneticsSimulator,
              entityDetails.baseEntityInfo.asInstanceOf[AnimalInfo])
            )
            currentWatched = Some(entity.id)
            mainComponent.watchEntity(currentWatched.get)
          case _=>
            if(currentWatched.isDefined){
              mainComponent.unwatchEntity(currentWatched.get)
              currentWatched = None
            }
        }
        currentSelected = Some(entity.id)
        showSelectedEntity(pos, entity)
        detailsPane.showDetails(entity,entityDetails)

      case None =>
        if(currentWatched.isDefined){
          mainComponent.unwatchEntity(currentSelected.get)
        }
        currentWatched = None
        currentSelected = None
        detailsPane.clearDetails()
        genomeDetailsPane.clearGenomeStats()
    }


  }


  filterEvent(ScrollEvent.Any) {

    (e: ScrollEvent) => {
      if(e.controlDown) {
        val value = if(e.deltaY>0) 1 else -1
        mainScene.zoomSlider.value()+=value

        e.consume()
      }
    }
  }


  override def updateWorld(generation: Int, world: List[Entity]): Unit = {
    drawWorld(world)
  }

  private def drawWorld(world: List[Entity]): Unit = {

    currentWorld = Map() ++ world.map(e => (Position(e.position.x * entitySize(), e.position.y * entitySize()), e))

    Platform.runLater {
      () => {

        graphicsContext.clearRect(0, 0, worldViewWidth(), worldViewHeigth())
        world foreach (e => {
          drawEntity(Position(e.position.x * entitySize(), e.position.y * entitySize()), e.color)
        })
        currentSelected match {
          case Some(id) =>
            if(getEntityById(id).isDefined){
              val entity: Entity = getEntityById(id).get
              val position: Position = getEntityViewPosition(entity)
              val entityDetails:model.EntityInfo = mainComponent.getEntityDetails(entity.id).get
              entityDetails.baseEntityInfo match {
                case AnimalInfo(_,_,_,_,_,_) =>
                  genomeDetailsPane.setGenomeStats(GenomeStatsUtilities.buildGenomeStats(
                    geneticsSimulator,
                    entityDetails.baseEntityInfo.asInstanceOf[AnimalInfo])
                  )
                case _=>
              }
              detailsPane.showDetails(entity,entityDetails)
              currentSelected = Some(entity.id)
              showSelectedEntity(position, entity)
            }else{
              currentSelected = None
              detailsPane.clearDetails()
              genomeDetailsPane.clearGenomeStats()

            }

          case None =>
        }
      }
    }


  }

  private def showSelectedEntity(position: Position, entity: Entity): Unit = {
    drawEntity(position, selectionColor)
    val ed: EntityInfo = mainComponent.getEntityDetails(entity.id).get
    ed.baseEntityInfo match {
      case a: AnimalInfo => showVisualField(position, ed.visualField)
      case _ =>
    }
  }

  private def clearSelectedEntity(position: Position, entity: Entity): Unit = {
    drawEntity(position, entity.color)
    val ed: EntityInfo = mainComponent.getEntityDetails(entity.id).get
    ed.baseEntityInfo match {
      case a: AnimalInfo => clearVisualField(position, ed.visualField)
      case _ =>
    }
  }

  private def drawEntity(position: Position, color: Color): Unit = {
    graphicsContext.fill = color
    graphicsContext.fillOval(position.x, position.y, entitySize(), entitySize())
  }

  private def showVisualField(position: Position, visualField: Double): Unit = {
    drawVisualField(position, selectionColor, visualField)
  }

  private def clearVisualField(position: Position, visualField: Double): Unit = {
    drawVisualField(position, Color.White, visualField)
  }

  private def drawVisualField(position: Position, color: Color, visualField: Double): Unit = {
    graphicsContext.stroke = color
    val centerPosition = Position(position.x-entitySize()*(visualField/2), position.y-entitySize()*(visualField/2))
    graphicsContext.strokeOval(centerPosition.x, centerPosition.y, entitySize()*visualField, entitySize()*visualField)
  }

  private def getEntityById(id: String): Option[Entity] = {
    currentWorld.values.find(e => e.id==id)
  }

  private def getEntityViewPosition(entity: Entity): Position = {
    Position(entity.position.x*entitySize(), entity.position.y*entitySize())
  }

  private def getEntityViewStartPosition(x: Double, y: Double): Position = {
    val posx = x.toInt - (x.toInt%entitySize())
    val posy = y.toInt - (y.toInt%entitySize())
    Position(posx, posy)
  }

  private def getEntityRealPosition(x: Double, y: Double): Position = {
    val pos: Position = getEntityViewStartPosition(x, y)
    Position(pos.x/entitySize(), pos.y/entitySize())
  }
}
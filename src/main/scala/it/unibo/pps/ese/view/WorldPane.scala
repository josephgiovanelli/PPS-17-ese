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
import it.unibo.pps.ese.genericworld.model.{EntityInfo, EntityState, ReignType}
import javafx.application.Platform
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent, ScrollEvent}
import scalafx.scene.paint.Color
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._
import it.unibo.pps.ese.view.bodyViewer.BodyPane
import Color._
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityKinds
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._

import scala.util.Random

trait WorldView {
  def updateWorld(generation: Int, world: Seq[EntityState]): Unit
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


  val backGroundColor: Color = valueOf("#2c3e50")
  val selectionColor: Color = Yellow

  val plantColors: List[Color] = List(
    Green,
    LawnGreen,
    ForestGreen
  )


  val animalColors: List[Color] = List(
    Red,
    LightGrey,
    Orange,
    SandyBrown,
    DeepPink,
    BlueViolet,
    Magenta,
    DeepSkyBlue
  )


  var animalColorPool: List[Color] = animalColors
  val animalColorMap: Map[String, Color] = getAnimalColors

  var plantColorPool: List[Color] = plantColors
  val plantColorMap: Map[String, Color] = getPlantColors

  var currentWorld: Map[Position, EntityState] = Map()
  var currentSelected: Option[String] = None
  var currentWatched: Option[String] = None
  var worldViewWidth: DoubleProperty = DoubleProperty(worldWidth*entitySize())
  var worldViewHeigth: DoubleProperty = DoubleProperty(worldHeigth*entitySize())
  worldViewWidth <== entitySize*worldWidth
  worldViewHeigth <== entitySize*worldHeigth

  val canvas = new Canvas()
  canvas.width <== worldViewWidth
  canvas.height <== worldViewHeigth

  val graphicsContext: GraphicsContext = canvas.graphicsContext2D
  graphicsContext.fill = backGroundColor
  graphicsContext.fillRect(0, 0, worldViewWidth(), worldViewHeigth())

  content = canvas

  entitySize.onChange(drawWorld(currentWorld.values.toList))

  tooltip = new Tooltip()

  canvas.onMouseMoved = (e: MouseEvent) => {
    val pos: Position = getEntityViewStartPosition(e.x, e.y)
    currentWorld.get(pos) match {
      case Some(entity) =>
        tooltip().text = entity.state.species.toString
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
        val entity: EntityState = getEntityById(id).get
        val position: Position = getEntityViewPosition(entity)
        clearSelectedEntity(position, entity)
      case None =>
    }

    val pos: Position = getEntityViewStartPosition(e.x, e.y)
    currentWorld.get(pos) match {
      case Some(entity) =>
        entity.state.baseEntityInfo match {
          case AnimalInfo(_,_,_,_,_,_) =>
            genomeDetailsPane.setGenomeStats(GenomeStatsUtilities.buildGenomeStats(
              geneticsSimulator,
              entity.state.baseEntityInfo.asInstanceOf[AnimalInfo])
            )
            currentWatched = Some(entity.entityId)
            mainComponent.watchEntity(currentWatched.get)
          case _=>
            if(currentWatched.isDefined){
              mainComponent.unwatchEntity(currentWatched.get)
              currentWatched = None
            }
        }
        currentSelected = Some(entity.entityId)
        showSelectedEntity(pos, entity)
        detailsPane.showDetails(entity)

      case None =>
        if(currentWatched.isDefined){
          mainComponent.unwatchEntity(currentWatched.get)
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


  override def updateWorld(generation: Int, world: Seq[EntityState]): Unit = {
    drawWorld(world)
  }

  private def drawWorld(world: Seq[EntityState]): Unit = {

    currentWorld = Map() ++ world.map(e => (Position(e.state.position.x * entitySize(), e.state.position.y * entitySize()), e))

    Platform.runLater {
      () => {

        graphicsContext.fill = backGroundColor
        graphicsContext.fillRect(0, 0, worldViewWidth(), worldViewHeigth())
        world foreach (e => {

          e.state.reign match {
            case ReignType.PLANT =>
              drawEntity(Position(e.state.position.x * entitySize(), e.state.position.y * entitySize()), plantColorMap(e.state.species.toString))
            case ReignType.ANIMAL =>
              drawEntity(Position(e.state.position.x * entitySize(), e.state.position.y * entitySize()), animalColorMap(e.state.species.toString))
          }

        })
        currentSelected match {
          case Some(id) =>
            if(getEntityById(id).isDefined){
              val entity: EntityState = getEntityById(id).get
              val position: Position = getEntityViewPosition(entity)
              entity.state.baseEntityInfo match {
                case AnimalInfo(_,_,_,_,_,_) =>
                  genomeDetailsPane.setGenomeStats(GenomeStatsUtilities.buildGenomeStats(
                    geneticsSimulator,
                    entity.state.baseEntityInfo.asInstanceOf[AnimalInfo])
                  )
                case _=>
              }
              detailsPane.showDetails(entity)
              currentSelected = Some(entity.entityId)
              showSelectedEntity(position, entity)
            }else{
              currentSelected = None
              detailsPane.clearDetails()
              genomeDetailsPane.clearGenomeStats()
              if(currentWatched.isDefined){
                mainComponent.unwatchEntity(currentWatched.get)
                currentWatched = None
              }
            }

          case None =>
        }
      }
    }


  }

  private def showSelectedEntity(position: Position, entity: EntityState): Unit = {
    drawEntity(position, selectionColor)
    entity.state.reign match {
      case ReignType.ANIMAL => showVisualField(position, entity.state.visualField)
      case _ =>
    }
  }

  private def clearSelectedEntity(position: Position, entity: EntityState): Unit = {
    entity.state.reign match {
      case ReignType.PLANT => drawEntity(position, plantColorMap(entity.state.species.toString))
      case ReignType.ANIMAL => drawEntity(position, animalColorMap(entity.state.species.toString))
    }

    entity.state.reign match {
      case ReignType.ANIMAL => clearVisualField(position, entity.state.visualField)
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
    drawVisualField(position, backGroundColor, visualField)
  }

  private def drawVisualField(position: Position, color: Color, visualField: Double): Unit = {
    graphicsContext.stroke = color
    val centerPosition = Position(position.x-entitySize()*(visualField/2), position.y-entitySize()*(visualField/2))
    graphicsContext.strokeOval(centerPosition.x, centerPosition.y, entitySize()*visualField, entitySize()*visualField)
  }

  private def getEntityById(id: String): Option[EntityState] = {
    currentWorld.values.find(e => e.entityId==id)
  }

  private def getEntityViewPosition(entity: EntityState): Position = {
    Position(entity.state.position.x*entitySize(), entity.state.position.y*entitySize())
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

  private def getAnimalColors: Map[String, Color] = {

    geneticsSimulator.speciesList.map(s => (s, {
      if (animalColorPool.isEmpty) animalColorPool=animalColors
      val r = Random.nextInt(animalColorPool.size)
      val c = animalColorPool(r)
      animalColorPool = animalColorPool diff List(c)
      c
    })).toMap
  }

  private def getPlantColors: Map[String, Color] = {
    geneticsSimulator.plantSpeciesList.map(s => (s, {
      if (plantColorPool.isEmpty) plantColorPool=plantColors
      val r = Random.nextInt(plantColorPool.size)
      val c = plantColorPool(r)
      plantColorPool = plantColorPool diff List(c)
      c
    })).toMap
  }
}
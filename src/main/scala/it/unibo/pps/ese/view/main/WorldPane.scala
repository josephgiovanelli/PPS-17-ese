package it.unibo.pps.ese.view.main

import java.awt.MouseInfo

import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityState
import javafx.application.Platform
import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo
import it.unibo.pps.ese.utils.Position
import it.unibo.pps.ese.view.core._
import it.unibo.pps.ese.view.sections.filters.EntityFiltersValues
import it.unibo.pps.ese.view.main.WorldPrefernces._
import it.unibo.pps.ese.view.sections.speciesdetails.{GenomeDetailsPane, GenomeStatsUtilities}

import scala.util.Random
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.{ScrollPane, Tooltip}
import scalafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._

/**
  * Contains the methods relative to the world pane visible from outside
  */
trait WorldView {
  /**
    * Updates the world and shows the new state
    *
    * @param generation the current generation
    * @param world the new state of the world
    */
  def updateWorld(generation: Int, world: Seq[EntityState]): Unit
}

/**
  * COntains all the methods that allows the communication between view's components
  */
trait WorldPane extends ScrollPane with WorldView{
  var entitySize: IntegerProperty = IntegerProperty(ZoomPreferences.prefZoom)

  /**
    * Applies the given filters to the wolrd view
    *
    * @param entityFiltersValues the filters to be applied
    */
  def applyFilters(entityFiltersValues: EntityFiltersValues): Unit

  /**
    * Clears the filters
    */
  def clearFilters(): Unit
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

  var entityFiltersValues: Option[EntityFiltersValues] = None

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
  var animalColorMap: Map[String, Color] = getAnimalColors

  var plantColorPool: List[Color] = plantColors
  var plantColorMap: Map[String, Color] = getPlantColors

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

  entitySize.onChange(drawWorld(currentWorld.values.toList, entityFiltersValues))

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
        detailsPane.showDetails(entity,currentSelected. get)

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
    val newColors:Map[String,Color] = geneticsSimulator.plantSpeciesList.filter(!plantColorMap.contains(_)).map(s=>s->getPlantColorOfPool).toMap
    plantColorMap ++= newColors
    val newAnimalColors:Map[String,Color] = geneticsSimulator.speciesList.filter(!animalColorMap.contains(_)).map(s=>s->getAnimalColorOfPool).toMap
    animalColorMap ++= newAnimalColors
    drawWorld(world, entityFiltersValues)
  }

  private def drawWorld(world: Seq[EntityState], entityFiltersValues: Option[EntityFiltersValues]): Unit = {

    import it.unibo.pps.ese.view.utilities.EntityConversions._

    val filtersOn: Boolean = entityFiltersValues match {
      case Some(_) => true
      case None => false
    }

    currentWorld = Map() ++ world.map(e => (Position(e.state.position.x * entitySize(), e.state.position.y * entitySize()), e))

    Platform.runLater {
      () => {

        graphicsContext.fill = backGroundColor
        graphicsContext.fillRect(0, 0, worldViewWidth(), worldViewHeigth())
        world foreach (e => {

          e.state.reign match {
            case ReignType.PLANT =>
              val draw: Boolean = entityFiltersValues match {
                case None => true
                case Some(f) => f.reign match {
                  case Some(ReignType.PLANT) => e.state.applyFilter(f)
                  case _ => false
                }
              }
              if (draw) {
                drawEntity(Position(e.state.position.x * entitySize(), e.state.position.y * entitySize()), plantColorMap(e.state.species.toString))
              }
            case ReignType.ANIMAL =>
              val draw: Boolean = entityFiltersValues match {
                case None => true
                case Some(f) => f.reign match {
                  case Some(ReignType.ANIMAL) => e.state.applyFilter(f)
                  case _ => false
                }
              }
              if (draw) {
                drawEntity(Position(e.state.position.x * entitySize(), e.state.position.y * entitySize()), animalColorMap(e.state.species.toString))
              }
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
              detailsPane.showDetails(entity,currentSelected.get)
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
      case ReignType.ANIMAL => showFields(position, entity.state.visualField, entity.state.actionField)
      case _ =>
    }
  }

  private def clearSelectedEntity(position: Position, entity: EntityState): Unit = {
    entity.state.reign match {
      case ReignType.PLANT => drawEntity(position, plantColorMap(entity.state.species.toString))
      case ReignType.ANIMAL => drawEntity(position, animalColorMap(entity.state.species.toString))
    }

    entity.state.reign match {
      case ReignType.ANIMAL => clearFields(position, entity.state.visualField, entity.state.actionField)
      case _ =>
    }
  }

  private def drawEntity(position: Position, color: Color): Unit = {
    graphicsContext.fill = color
    graphicsContext.fillOval(position.x, position.y, entitySize(), entitySize())
  }

  private def showFields(position: Position, visualField: Double, actionField: Double): Unit = {
    drawFields(position, selectionColor, visualField, actionField)
  }

  private def clearFields(position: Position, visualField: Double, actionField: Double): Unit = {
    drawFields(position, backGroundColor, visualField, actionField)
  }

  private def drawFields(position: Position, color: Color, visualField: Double, actionField: Double): Unit = {
    graphicsContext.stroke = color
    val visualFieldCenterPosition = Position(position.x-entitySize()*(visualField/2), position.y-entitySize()*(visualField/2))
//    val actionFieldCenterPosition = Position(position.x-entitySize()*(actionField/2), position.y-entitySize()*(actionField/2))
    graphicsContext.strokeOval(visualFieldCenterPosition.x, visualFieldCenterPosition.y, entitySize()*visualField, entitySize()*visualField)
//    graphicsContext.strokeOval(actionFieldCenterPosition.x, actionFieldCenterPosition.y, entitySize()*actionField, entitySize()*actionField)
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
        getAnimalColorOfPool
    })).toMap
  }

  def getAnimalColorOfPool:Color = {
    val r = Random.nextInt(animalColorPool.size)
    val c = animalColorPool(r)
    animalColorPool = animalColorPool diff List(c)
    c
  }
  def getPlantColorOfPool:Color = {
    val r = Random.nextInt(plantColorPool.size)
    val c = plantColorPool(r)
    plantColorPool = plantColorPool diff List(c)
    c
  }
  private def getPlantColors: Map[String, Color] = {
    geneticsSimulator.plantSpeciesList.map(s => (s, {
      if (plantColorPool.isEmpty) plantColorPool=plantColors
      getPlantColorOfPool
    })).toMap
  }

  override def applyFilters(e: EntityFiltersValues): Unit = {
    entityFiltersValues = Some(e)
    drawWorld(currentWorld.values.toList, entityFiltersValues)
  }

  override def clearFilters(): Unit = {
    entityFiltersValues = None
    drawWorld(currentWorld.values.toList, None)
  }
}

object WorldPrefernces {
  val worldWidth: Int = 500
  val worldHeigth: Int = 500
}
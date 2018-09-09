package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Position
import it.unibo.pps.ese.controller.loader.data.SimulationData
import it.unibo.pps.ese.genericworld.model.{EntityInfo, EntityInfoConversion, EntityState}
import it.unibo.pps.ese.utils.Point
import it.unibo.pps.ese.view.Entity
import scalafx.scene.paint.Color

case class EntityDetails(id: String, species: String, position: Position)

trait Observer {
  def getEntityDetails(id: String): Option[EntityInfo]
  def setWatched(id: String): Unit
}

//class View {
//
//  var frame : Int = 0
//
//  def addObserver(observer: Observer): Unit = Unit
//
//  def render(viewData: ViewData): Unit = {
//    (viewData entities) take 1 foreach(e => println("Frame: " + frame + ", Id: " + e.id
//      + ", Species: " + e.species + ", Position: " + e.position + ", Color: " + e.color))
//    frame += 1
//  }
//}

object ViewHelpers {
  import EntityInfoConversion._

  implicit def intToRGBA(i: Int): String = Integer.toHexString((i >> 24) & 0xFF) + Integer.toHexString((i >> 16) & 0xFF) + Integer.toHexString((i >> 8) & 0xFF) + Integer.toHexString(i & 0xFF)

  implicit def toPosition(point: Point): Position = Position(point x, point y)

  implicit def intRoRgbColor(i: Int): Color = Color.rgb((i >> 24) & 0xFF, (i >> 16) & 0xFF, (i >> 8) & 0xFF, 1.0)

  implicit def toEntityViewData(data: EntityState): Entity =
    Entity(data.entityId, data.state.species.toString, toPosition(data.state.position), data.state.species.toString.hashCode)

  implicit def toEntityViewDetails(data: EntityState): EntityDetails =
    EntityDetails(data.entityId, data.state.species.toString, toPosition(data.state.position))

  implicit def toViewData(data: Seq[EntityState]): List[Entity] =
    (data map toEntityViewData).toList

  implicit class ManageableObserver(manageableController: ManageableController) extends Observer {
    override def getEntityDetails(id: String): Option[EntityInfo] = (manageableController entityData id) map(_.state.copy())

    override def setWatched(id: String): Unit = manageableController watch id
  }
}

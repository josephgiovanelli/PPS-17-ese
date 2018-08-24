package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.genericworld.model.{EntityInfoConversion, EntityState}
import it.unibo.pps.ese.utils.Point

case class ViewData(entities : Iterable[EntityViewData])
case class EntityViewData(id: String, position: Point, species: String, color: String)

case class View(controller: ManageableController) {

  var frame : Int = 0

  def render(viewData: ViewData): Unit = {
    (viewData entities) take 1 foreach(e => println("Frame: " + frame + ", Id: " + e.id
      + ", Species: " + e.species + ", Position: " + e.position + ", Color: " + e.color))
    frame += 1
  }
}

object ViewHelpers {
  import EntityInfoConversion._

  implicit def intToARGB(i: Int): String = Integer.toHexString((i >> 24) & 0xFF) + Integer.toHexString((i >> 16) & 0xFF) + Integer.toHexString((i >> 8) & 0xFF) + Integer.toHexString(i & 0xFF)

  implicit def toViewData(data: Seq[EntityState]): ViewData =
    ViewData(data map(x => EntityViewData(x.entityId, x.state.position, x.state.species, x.state.species.hashCode)))
}

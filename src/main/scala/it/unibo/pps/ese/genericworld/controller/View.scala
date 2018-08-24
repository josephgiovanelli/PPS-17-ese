package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.genericworld.model.{EntityInfoConversion, EntityState}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class View(controller: ManageableController) {

  var frame : Int = 0

  def render(entities: Seq[EntityState]): Unit = {
    import EntityInfoConversion._
    Future {
      entities foreach(e => println("Frame: " + frame + ", Id: " + e.entityId + ", Speed: " + e.state.position))
      frame += 1
    }
  }
}

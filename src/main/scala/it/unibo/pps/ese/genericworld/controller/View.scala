package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.genericworld.model.{EntityInfoConversion, EntityState}

class View {

  import EntityInfoConversion._

  def render(entities: Seq[EntityState]): Unit = entities foreach(e => println("Id: " + e.entityId + ", Speed: " + e.state.speed))
}

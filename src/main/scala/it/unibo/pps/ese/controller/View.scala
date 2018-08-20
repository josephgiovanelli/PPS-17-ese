package it.unibo.pps.ese.controller

import it.unibo.pps.ese.model.{EntityInfoConversion, EntityState}

class View {

  import EntityInfoConversion._

  def render(entities: Seq[EntityState]): Unit = entities foreach(e => println("Id: " + e.entityId + ", Speed: " + e.state.speed))
}

package it.unibo.pps.ese.controller.simulation.runner.core.data

import scala.language.dynamics

class EntityInfo extends Dynamic {

  private var values: Map[String, Any] = Map.empty[String, Any]

  def selectDynamic(name: String): Any = {
    values.get(name) match {
      case Some(value) => value
      case None => None
    }
  }

  def updateDynamic(name: String)(value: Any): Unit = {
    values = values + (name -> value)
  }

  def copy(): EntityInfo = {
    val copy = new EntityInfo()
    copy values = values
    copy
  }
}
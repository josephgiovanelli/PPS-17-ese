package it.unibo.pps.ese.controller.simulation.runner.core.data

import scala.language.dynamics

/**
  * This dynamic class is used to store the public data associated with an entity.
  */
class EntityInfo extends Dynamic {

  private var values: Map[String, Any] = Map.empty[String, Any]

  /**
    * Get a property value
    * @param name The property name
    * @return The property value
    */
  def selectDynamic(name: String): Any = {
    values.get(name) match {
      case Some(value) => value
      case None => None
    }
  }

  /**
    * Set a property value
    * @param name The property name
    * @param value The property value
    */
  def updateDynamic(name: String)(value: Any): Unit = {
    values = values + (name -> value)
  }

  /**
    * Make a copy of the EntityInfo.
    * @return The requested copy
    */
  def copy(): EntityInfo = {
    val copy = new EntityInfo()
    copy values = values
    copy
  }
}
package it.unibo.pps.ese.genericworld.model

import scala.language.dynamics
import scala.collection.mutable

class EntityInfo extends Dynamic {

  val values: mutable.Map[String, Any] = mutable.Map.empty[String, Any]

  def selectDynamic(name: String): Any = {
    values.get(name) match {
      case Some(value) => value
      case None => None
    }
  }

  def updateDynamic(name: String)(value: Any): Unit = {
    values(name) = value
  }
}
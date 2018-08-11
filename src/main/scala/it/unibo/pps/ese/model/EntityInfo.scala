package it.unibo.pps.ese.model

import scala.language.dynamics
import scala.collection.mutable

//Aggiungere qua filtro era

class EntityInfo extends Dynamic {

  val values: mutable.Map[String, StoredProperty] = mutable.Map.empty[String, StoredProperty]

  def selectDynamic(name: String): Any = {
    values.get(name) match {
      case Some(value) => value
      case None => None
    }
  }

  def updateDynamic(name: String)(value: StoredProperty): Unit = {
    values(name) = value
  }
}
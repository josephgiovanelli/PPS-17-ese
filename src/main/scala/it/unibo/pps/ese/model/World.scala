package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.DataRepository

sealed trait World {
  def getEntities : Seq[Entity]
  def addEntity(entity : Entity) : Unit
}

sealed trait CachedWorld[A, B] {
  def entitiesStateCache : DataRepository[A, B]
}

object World {

  def apply(): World = new BaseWorld

  private case class BaseWorld() extends World {
    private[this] var entities : Seq[Entity] = Seq.empty

    override def getEntities: Seq[Entity] = entities
    override def addEntity(entity: Entity): Unit = entities = entities :+ entity
  }

  //private trait BaseCachedWorld extends CachedWorld [String, DataRepository[]]
}


//class EntitiesStateCache
package it.unibo.pps.ese.model

sealed trait World {
  def entities : Seq[Entity]
}

sealed trait CachedWorld {
  def queryableState : EntitiesStateCache = EntitiesStateCache apply
}

sealed trait InteractiveWorld extends CachedWorld {
  def addEntity(entity: Entity): Unit
  def removeEntity(id: String): Unit
}


object World {

  def apply(): InteractiveWorld = new BaseInteractiveWorld

  private case class BaseWorld() extends World {

    private[this] var _entities : Seq[Entity] = Seq empty

    protected def entities_=(entities : Seq[Entity]) : Unit = _entities = entities
    override def entities: Seq[Entity] = _entities
  }

  private class BaseInteractiveWorld extends BaseWorld with InteractiveWorld {

    override def addEntity(entity: Entity): Unit = {
      entity match {
        case _: Entity with BaseNervousSystem => entity addComponent WorldBridgeComponent(this)
      }
      entities_=(entities :+ entity)
    }

    override def removeEntity(id: String): Unit = entities_=(entities filterNot(e => e.id == id))
  }
}
package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.Done

import scala.concurrent.Future

sealed trait World {
  def entities : Seq[Entity]
}

sealed trait CachedWorld {
  private[this] val _queryableState = EntitiesStateCache apply
  def queryableState : EntitiesStateCache = _queryableState
}

sealed trait InteractiveWorld extends CachedWorld {
  def addEntity(entity: Entity): Unit
  def removeEntity(id: String): Unit
}

//Boh boh

sealed trait UpdatableWorld {
  private[this] var _entityBridges : Seq[WorldBridgeComponent] = Seq empty
  def addBridge(bridge : WorldBridgeComponent): Unit = _entityBridges = _entityBridges :+ bridge
  def removeBridge(entityId: String): Unit = _entityBridges = _entityBridges filterNot(bridge => bridge.entitySpecifications.id == entityId)
  def requireStateUpdate: Future[Done]
  def requireInfoUpdate: Future[Done]
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
        case _: Entity with NervousSystemExtension => entity addComponent new WorldBridgeComponent(entity specifications, this)
        case _ => Unit
      }
      entities_=(entities :+ entity)
    }

    override def removeEntity(id: String): Unit = entities_=(entities filterNot(e => e.id == id))
  }
}
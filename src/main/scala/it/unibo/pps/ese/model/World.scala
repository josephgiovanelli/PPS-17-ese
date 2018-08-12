package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.Done
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

sealed trait World {
  def addEntity(entity: Entity): Unit
  def removeEntity(id: String): Unit
  def entitiesState : Seq[EntityState]
  def requireStateUpdate: Future[Done]
  def requireInfoUpdate: Future[Done]
}

sealed trait CachedWorld {
  private[this] val _queryableState = EntitiesStateCache apply
  def queryableState : EntitiesStateCache = _queryableState
}

sealed trait InteractiveWorld extends CachedWorld {
  def entities : Seq[Entity]
  def addEntity(entity: Entity): Unit
  def removeEntity(id: String): Unit
}

sealed trait UpdatableWorld {

  private[this] var _entityBridges : Seq[WorldBridgeComponent] = Seq empty
  def addBridge(bridge : WorldBridgeComponent): Unit = _entityBridges = _entityBridges :+ bridge
  def removeBridge(entityId: String): Unit = _entityBridges = _entityBridges filterNot(bridge => bridge.entitySpecifications.id == entityId)
  def requireStateUpdate: Future[Done] = serializeFutures(_entityBridges)(e => e.computeNewState()) map(_ => new Done())
  def requireInfoUpdate: Future[Done] = Future.traverse(_entityBridges)(e => e.requireInfo()) map (_ => new Done())

  private def serializeFutures[A, B](l: Iterable[A])(fn: A ⇒ Future[B]): Future[List[B]] =
    l.foldLeft(Future(List.empty[B])) {
      (previousFuture, next) =>
        for {
          previousResults <- previousFuture
          next <- fn(next)
        } yield previousResults :+ next
    }
}

object World {

  def apply(): World = new BaseInteractiveWorld

  private class BaseInteractiveWorld extends World with InteractiveWorld with UpdatableWorld {

    private[this] var _entities : Seq[Entity] = Seq empty

    protected def entities_=(entities : Seq[Entity]) : Unit = _entities = entities
    override def entities: Seq[Entity] = _entities

    override def addEntity(entity: Entity): Unit = {
      entity match {
        case _: Entity with NervousSystemExtension =>
          val bridge = new WorldBridgeComponent(entity specifications, this)
          addBridge(bridge)
          entity addComponent bridge
        case _ => Unit
      }
      entities_=(entities :+ entity)
    }

    override def removeEntity(id: String): Unit = {
      removeBridge(id)
      entities_=(entities filterNot(e => e.id == id))
    }

    override def entitiesState: Seq[EntityState] = queryableState getFilteredState(_ => true)
  }
}
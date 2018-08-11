package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.BaseEvent

case class RequireEntitiesState(id: String, filter: EntityState => Boolean = _ => true) extends BaseEvent
case class EntitiesStateResponse(id: String, state: Seq[EntityState]) extends BaseEvent
case class CreateEntity(entity: Entity) extends BaseEvent
case class KillEntity(entityId: String) extends BaseEvent
case class NewState(properties: Seq[EntityProperty]) extends BaseEvent

class WorldBridgeComponent(override val entityId: String, world: InteractiveWorld) extends WriterComponent(entityId) {

  override def initialize(): Unit = subscribe {
    case RequireEntitiesState(id, filter) => publish(EntitiesStateResponse(id, (world queryableState) getFilteredState filter))
    case CreateEntity(entity) => world addEntity entity
    case KillEntity(`entityId`) => world removeEntity entityId
    case NewState(properties) => properties foreach (e => (world queryableState) addOrUpdateEntityState (entityId, e))
    case _ => Unit
  }
}

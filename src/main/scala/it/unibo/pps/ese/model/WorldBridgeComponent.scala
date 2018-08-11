package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.BaseEvent

case class RequireEntitiesState(filter: EntityState => Boolean = _ => true) extends BaseEvent
case class CreateEntity(entity: Entity) extends BaseEvent
case class KillEntity(entityId: String) extends BaseEvent

case class WorldBridgeComponent(world: InteractiveWorld) extends ReaderComponent {

  override def initialize(): Unit = subscribe {
    case RequireEntitiesState(filter) => (world queryableState) getFilteredState filter
    case CreateEntity(entity) => world addEntity entity
    case KillEntity(entityId) => world removeEntity entityId
  }
}

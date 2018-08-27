package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.genericworld.model.support.DataRepository

case class EntityState(entityId: String, state: EntityInfo)
case class EntityProperty(propertyId : String, value : Any)

sealed trait EntitiesStateCache {

  def addOrUpdateEntityState (entityId: String, element: EntityProperty) : Unit
  def getEntityState(entityId: String): EntityState
  def deleteEntityState(entityId: String) : Unit
  def getFilteredState(filter: EntityState => Boolean ) : Seq[EntityState]

}

object EntitiesStateCache {

  def apply: EntitiesStateCache = DynamicEntitiesStateCache()

  private case class DynamicEntitiesStateCache() extends EntitiesStateCache {

    private val _entitiesRepository : DataRepository[String, EntityInfo] =
      DataRepository[String, EntityInfo]

    override def addOrUpdateEntityState(entityId: String, element: EntityProperty): Unit = this synchronized {
      val entityState = _entitiesRepository getById entityId getOrElse new EntityInfo
      entityState.updateDynamic(element.propertyId)(element value)
      _entitiesRepository addOrUpdate (entityId, entityState)
    }

    override def getEntityState(entityId: String): EntityState = this synchronized {
      EntityState(entityId, _entitiesRepository getById entityId getOrElse new EntityInfo)
    }

    override def deleteEntityState(entityId: String): Unit = this synchronized {
      _entitiesRepository deleteById entityId
    }

    override def getFilteredState(filter: EntityState => Boolean): Seq[EntityState] = this synchronized {
      val entityStates = (_entitiesRepository getAll) map(x => EntityState(x._1, x._2))
      entityStates filter filter
    }
  }
}

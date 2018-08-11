package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.DataRepository

case class EntityState(entityId: String, properties: Seq[EntityProperty])
case class EntityProperty(propertyId : String, era: Long, value : Object)
case class StoredProperty(era: Long, value : Object)

sealed trait EntitiesStateCache {

  def addOrUpdateEntityState (entityId: String, era: Int, element: EntityProperty) : Unit
  def getEntityState(entityId: String): EntityState
  def deleteEntityState(entityId: String) : Unit
  def getFilteredState(filter: EntityState => Boolean ) : Seq[EntityState]

}

object EntitiesStateCache {

  def apply: EntitiesStateCache = BaseEntitiesStateCache()

  private case class BaseEntitiesStateCache() extends EntitiesStateCache {

    private val _entitiesRepository : DataRepository[String, DataRepository[String, StoredProperty]] =
      DataRepository[String, DataRepository[String, StoredProperty]]

    override def addOrUpdateEntityState(entityId: String, era: Int, element: EntityProperty): Unit = {
      val entityState = _entitiesRepository getById entityId getOrElse DataRepository[String, StoredProperty]
      entityState addOrUpdate(element propertyId, StoredProperty(era = era, element value))
    }

    override def getEntityState(entityId: String): EntityState = {
      val entityState = if (_entitiesRepository exists entityId)
        (_entitiesRepository getById entityId getOrElse(throw new RuntimeException("Error")) getAll)
          .map(x => EntityProperty(x._1, x._2.era, x._2.value)) else Seq.empty[EntityProperty]
      EntityState(entityId, entityState)
    }

    override def deleteEntityState(entityId: String): Unit = _entitiesRepository deleteById entityId

    override def getFilteredState(filter: EntityState => Boolean): Seq[EntityState] = {
      println("richiesta stato")
      val entityStates = (_entitiesRepository getAll) map(x => EntityState(x._1, (x._2 getAll)
        .map(y => EntityProperty(y._1, y._2.era, y._2.value))))
      entityStates filter filter
    }
  }
}

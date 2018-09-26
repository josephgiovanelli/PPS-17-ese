package it.unibo.pps.ese.controller.simulation.runner.core.data

import it.unibo.pps.ese.controller.simulation.runner.core.support.DataRepository

case class EntityState(entityId: String, state: EntityInfo)
case class EntityProperty(propertyId : String, value : Any)

/**
  * Trait containing public APIs used to read in the entities' state repository data
  */
sealed trait ReadOnlyEntityState {

  /**
    * Get an entity public status
    * @param entityId The entity identifier
    * @return The entity state
    */
  def getEntityState(entityId: String): EntityState

  /**
    * Get all the data contained in the repository
    * @param filter A filter to be a pplied to the requested data
    * @return The requested data
    */
  def getFilteredState(filter: EntityState => Boolean ) : Seq[EntityState]
}

/**
  * Trait containing public APIs used to write in the entities' state repository data
  */
sealed trait EntitiesStateCache extends ReadOnlyEntityState {

  /**
    * Make publicly available an entity status. By default entity state is private
    * @param entityId The entity identifier
    */
  def publishEntityState(entityId: String)

  /**
    * Hide an entity status.
    * @param entityId The entity identifier
    */
  def hideEntityState(entityId: String)

  /**
    * Update the status of an entity
    * @param entityId The entity identifier
    * @param element The property to update
    */
  def addOrUpdateEntityState (entityId: String, element: EntityProperty) : Unit
}

object EntitiesStateCache {

  def apply: EntitiesStateCache = new DynamicEntitiesStateCache()

  /**
    * EntitiesStateCache implementation based on DataRepository and EntityInfo classes
    */
  private class DynamicEntitiesStateCache() extends EntitiesStateCache {

    private val _entitiesRepository : DataRepository[String, EntityInfo] =
      DataRepository[String, EntityInfo]

    override def publishEntityState(entityId: String): Unit = this synchronized {
      (_entitiesRepository getById entityId getOrElse(throw new IllegalStateException("No item to publish"))).public = true
    }

    override def hideEntityState(entityId: String): Unit = this synchronized {
      (_entitiesRepository getById entityId getOrElse(throw new IllegalStateException("No item to hide"))).public = false
    }

    override def addOrUpdateEntityState(entityId: String, element: EntityProperty): Unit = this synchronized {
      val entityState = _entitiesRepository getById entityId getOrElse cleanState
      entityState.updateDynamic(element.propertyId)(element value)
      _entitiesRepository addOrUpdate (entityId, entityState)
    }

    override def getEntityState(entityId: String): EntityState = this synchronized {
      EntityState(entityId,
        _entitiesRepository getById entityId getOrElse(throw new IllegalStateException("No item found")) copy())
    }

    override def getFilteredState(filter: EntityState => Boolean): Seq[EntityState] = this synchronized {
      val entityStates = (_entitiesRepository getAll) filter (x => x._2.public == true) map(x => EntityState(x._1, x._2 copy()))
      entityStates filter filter
    }

    private def cleanState: EntityInfo = {
      val state = new EntityInfo
      state.public = false
      state
    }
  }
}

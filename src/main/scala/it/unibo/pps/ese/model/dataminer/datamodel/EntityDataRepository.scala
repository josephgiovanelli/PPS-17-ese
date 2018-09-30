package it.unibo.pps.ese.model.dataminer.datamodel

import it.unibo.pps.ese.controller.simulation.runner.core.support.DataRepository
import it.unibo.pps.ese.model.dataminer.DataModelSupport.{EntityId, Era}

/**
  * Trait containing public APIs used to read in the entities' consolidated data
  */
sealed trait ReadOnlyEntityRepository {
  protected var _newDataListeners: Seq[Era => Unit] = Seq empty

  /**
    * Check if the repository contains data about the given entity
    * @param entityId The entity identifier
    * @return True if data exists, false otherwise
    */
  def exists(entityId: EntityId): Boolean

  /**
    * Get the data about the living entities in the given era
    * @param era The chosen era
    * @return The requested data
    */
  def entitiesInEra(era: Era): Seq[EntityTimedRecord]

  /**
    * Get all the data available of an Entity
    * @param entityId The entity identifier
    * @return The entity data
    */
  def entityDynamicLog(entityId: EntityId): Option[EntityLog]

  /**
    * Get all the data contained in the repository
    * @return The requested data
    */
  def getAllDynamicLogs(): Seq[EntityLog]

  /**
    * Register a callback in order to be notified when new data is available
    * @param listener The callback to register
    */
  def attachNewDataListener(listener: Era => Unit): Unit = _newDataListeners = _newDataListeners :+ listener
}

/**
  * Trait containing public APIs used to write in the entities' consolidated data
  */
sealed trait EntityDataRepository extends ReadOnlyEntityRepository {

  /**
    * Update the static data of an entity
    * @param data The new data
    */
  def saveStaticEntityData(data: EntityStaticRecord): Unit

  /**
    * Update the dynamic data of an entity
    * @param era The era to which data refer
    * @param data The new data
    */
  def saveDynamicEntityData(era: Era, data: EntityDynamicRecord): Unit

  /**
    * Notify the data listeners that new data is available
    * @param era The era to which data refer
    */
  def generateNewDataNotification(era: Era): Unit = _newDataListeners foreach (_(era))
}

object EntityDataRepository {

  def apply(): EntityDataRepository = new BaseEntityDataRepository

  /**
    * EntityDataRepository implementation based on the data model defined in DataModel.
    * As memorization support DataRepository is used
    */
  private class BaseEntityDataRepository extends EntityDataRepository {

    private[this] val _dynamicLog: DataRepository[EntityId, EntityLog] =
      DataRepository[EntityId, EntityLog]

    override def saveStaticEntityData(data: EntityStaticRecord): Unit = this synchronized {

      def _prepareData: EntityLog = {
        val oldData = entityDynamicLog(data id)
        if (oldData isDefined) EntityLogImpl(oldData.get.id, data data, oldData.get.dynamicData)
        else EntityLogImpl(data id, data data, Seq empty)
      }

      _dynamicLog addOrUpdate (data id, _prepareData)
    }

    override def exists(entityId: EntityId): Boolean = this synchronized { _dynamicLog exists entityId }

    override def saveDynamicEntityData(era: Era, data: EntityDynamicRecord): Unit = this synchronized {
      _dynamicLog getById (data id) map(x =>
        EntityLogImpl(x id, x structuralData, x.dynamicData :+ (era, data.data))) foreach(x =>
        _dynamicLog addOrUpdate (data id, x))
    }

    override def entitiesInEra(era: Era): Seq[EntityTimedRecord] = this synchronized {
      (_dynamicLog getAll) filter (x => x._2.dynamicData.exists(y => y._1 == era)) map (x => x._2) map (x =>
        EntityTimedRecordImpl(x id, era, x structuralData, x.dynamicData.find(y => y._1 == era).map(y => y._2).get))
    }

    override def entityDynamicLog(entityId: EntityId): Option[EntityLog] =
      this synchronized { _dynamicLog getById entityId }

    override def getAllDynamicLogs(): Seq[EntityLog] =
      this synchronized { (_dynamicLog getAll) map (x => x._2) }
  }
}


package it.unibo.pps.ese.dataminer

import it.unibo.pps.ese.genericworld.model.support.DataRepository

sealed trait ReadOnlyEntityRepository {
  def exists(entityId: EntityId): Boolean
  def entitiesInEra(era: Era): Seq[EntityTimedRecord]
  def entityDynamicLog(entityId: EntityId): Option[EntityLog]
  def getAllDynamicLogs: Seq[EntityLog]
}

sealed trait EntityDataRepository extends ReadOnlyEntityRepository {
  def saveStaticEntityData(data: EntityStaticRecord): Unit
  def saveDynamicEntityData(era: Era, data: EntityDynamicRecord): Unit
}

object EntityDataRepository {

  def apply(): EntityDataRepository = new BaseEntityDataRepository

  private class BaseEntityDataRepository extends EntityDataRepository {

    private[this] val _dynamicLog: DataRepository[EntityId, EntityLog] =
      DataRepository[EntityId, EntityLog]

    override def saveStaticEntityData(data: EntityStaticRecord): Unit = {

      def _prepareData: EntityLog = {
        val oldData = entityDynamicLog(data id)
        if (oldData isDefined) EntityLogImpl(oldData.get.id, data data, oldData.get.dynamicData)
        else EntityLogImpl(data id, data data, Seq empty)
      }

      _dynamicLog addOrUpdate (data id, _prepareData)
    }

    override def exists(entityId: EntityId): Boolean = _dynamicLog exists entityId

    override def saveDynamicEntityData(era: Era, data: EntityDynamicRecord): Unit =
      _dynamicLog getById (data id) map(x =>
        EntityLogImpl(x id, x structuralData, x.dynamicData :+ (era, data.data))) foreach(x =>
        _dynamicLog addOrUpdate (data id, x))

    override def entitiesInEra(era: Era): Seq[EntityTimedRecord] =
      (_dynamicLog getAll) filter (x => x._2.dynamicData.exists(y => y._1 == era)) map (x => x._2) map (x =>
        EntityTimedRecordImpl(x id, era, x structuralData, x.dynamicData.find(y => y._1 == era).map(y => y._2).get))

    override def entityDynamicLog(entityId: EntityId): Option[EntityLog] = _dynamicLog getById entityId

    override def getAllDynamicLogs: Seq[EntityLog] = (_dynamicLog getAll) map (x => x._2)
  }
}


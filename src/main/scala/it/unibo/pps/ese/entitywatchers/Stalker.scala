package it.unibo.pps.ese.entitywatchers

import it.unibo.pps.ese.dataminer.{AnimalDynamicDataImpl, DynamicData, Era, ReadOnlyEntityRepository}
import it.unibo.pps.ese.utils.Point


case class Stalker(consolidatedState: ReadOnlyEntityRepository) {

  var stalked: Option[String] = None
  var currentEra: Long = 0
  var birthEra: Long = 0
  var deadEra: Option[Long] = None

  def stalk(entityId: String): Unit = {
    stalked = Some(entityId)
    birthEra = getBirthEra
    currentEra = birthEra
    deadEra = None
  }

  def unstalk: Unit =
    stalked = None

  def getEntities: Map[String, Point] =
    if (stalked.isDefined) {
      var result: Map[String, Point] = Map.empty
      var entities: Seq[String] = Seq.empty

      if (deadEra.isEmpty && isStalkedDeadInThisEra) {
        deadEra = Some(currentEra)
      }
      if (deadEra.isDefined && currentEra == deadEra.get) {
        currentEra = birthEra
      }

      getAllStalkedActions.foreach {
        case impl: AnimalDynamicDataImpl => entities ++= (impl.eating ++ impl.coupling ++ impl.givingBirth)
      }
      entities.foreach(entity => result += (entity -> getPositionInThisEra(entity)))
      currentEra += 1
      result
    } else {
      throw new IllegalStateException()
    }

  def getBirthEra: Era =
    consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._1).min

  def isStalkedDeadInThisEra: Boolean =
    !consolidatedState.entitiesInEra(currentEra).map(entity => entity.id).contains(stalked.get)

  def getAllStalkedActions: Seq[DynamicData] =
    consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._2)

  def getPositionInThisEra(entity: String): Point =
    consolidatedState.entitiesInEra(currentEra).filter(x => x.id == entity).head.dynamicData.position
}

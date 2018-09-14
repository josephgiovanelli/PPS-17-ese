package it.unibo.pps.ese.entitywatchers

import it.unibo.pps.ese.dataminer._
import it.unibo.pps.ese.genericworld.model.ReignType
import it.unibo.pps.ese.utils.Point

/**
  * Inform about what happend in the current era
  * @param me position of the stalked
  * @param actions the key is the action (eat, couple, give birth) and the value is the entities the stalked interacted with
  *                to do the key action
  * @param others all the entities with whom the stalked interacted, who are alive in this era
  */
case class ResultEra(me: Point, actions: Map[String, Seq[String]], others: Map[String, Point])

case class Stalker(consolidatedState: ReadOnlyEntityRepository) {

  var stalked: Option[String] = None
  var currentEra: Long = 0
  var birthEra: Long = 0
  var deadEra: Option[Long] = None
  var trueEra: Option[Long] = None

  def stalk(entityId: String): Unit = {
    if (stalked.isEmpty) {
      val entity: Option[EntityLog] = consolidatedState.entityDynamicLog(entityId)
      if (entity.isDefined && entity.get.structuralData.reign == ReignType.ANIMAL.toString) {
        stalked = Some(entityId)
        birthEra = getBirthEra
        currentEra = birthEra
        deadEra = None
      }
    }
  }


  def informAboutTrueEra(era: Long): Unit = {
    trueEra = Some(era)
  }

  def unstalk: Unit = {
    stalked = None
  }

  def report: ResultEra = {

    var me: Point = null
    var actions: Map[String, Seq[String]] = Map.empty
    var others: Map[String, Point] = Map.empty

    if (stalked.isDefined) {

      if (deadEra.isEmpty && isStalkedDeadInThisEra) {
      //if (deadEra.isEmpty && !consolidatedState.entitiesInEra(currentEra).map(entity => entity.id).contains(stalked.get)) {
        deadEra = Some(currentEra)
        println("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      }
      if ((deadEra.isDefined && currentEra >= deadEra.get) || (trueEra.isDefined && currentEra >= trueEra.get)) {
        currentEra = birthEra
      }

      var allInteractionEntitiesId: Seq[String] = Seq.empty
      getAllStalkedActions.foreach {
      //consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._2).foreach {
        case impl: AnimalDynamicDataImpl => allInteractionEntitiesId ++= (impl.eating ++ impl.coupling ++ impl.givingBirth)
      }

      allInteractionEntitiesId.foreach(entity =>
        if (consolidatedState.entitiesInEra(currentEra).exists(x => x.id == entity))
          others += (entity -> getPositionInThisEra(entity)))
          //others += (entity -> consolidatedState.entitiesInEra(currentEra).filter(x => x.id == entity).head.dynamicData.position))

      consolidatedState.entitiesInEra(currentEra).filter(x => x.id == stalked.get).head.dynamicData match {
        case impl: AnimalDynamicDataImpl =>
          me = impl.position
          actions += ("eat" -> impl.eating)
          actions += ("couple" -> impl.coupling)
          actions += ("give birth" -> impl.givingBirth)
      }

      if (trueEra.isDefined)
        println("!!!!!!!!!!!!!!!!!!!!!!!!" + (currentEra, trueEra.get))

      if (trueEra.isDefined && ((currentEra + 1) <= trueEra.get)) currentEra += 1

    }

    ResultEra(me, actions, others)
  }

  def getBirthEra: Era = {
    consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._1).min
  }

  def isStalkedDeadInThisEra: Boolean = {
    !consolidatedState.entitiesInEra(currentEra).map(entity => entity.id).contains(stalked.get)
  }

  def getAllStalkedActions: Seq[DynamicData] = {
    consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._2)
  }

  def getPositionInThisEra(entity: String): Point = {
    consolidatedState.entitiesInEra(currentEra).filter(x => x.id == entity).head.dynamicData.position
  }
}

package it.unibo.pps.ese.entitywatchers

import it.unibo.pps.ese.dataminer._
import it.unibo.pps.ese.genericworld.model.{EntityState, ReignType}
import it.unibo.pps.ese.utils.Point

/**
  * Inform about what happend in the current era
  * @param me position of the stalked
  * @param actions the key is the action (eat, couple, give birth) and the value is the entities the stalked interacted with
  *                to do the key action
  * @param others all the entities with whom the stalked interacted, who are alive in this era
  */
case class ResultEra(me: EntityTimedRecord, actions: Map[String, Seq[String]], others: Seq[ResultOther])

case class ResultOther(label: String, var entities: Map[String, Point])

case class Stalker(consolidatedState: ReadOnlyEntityRepository) {

  var stalked: Option[String] = None
  var currentEra: Long = 0
  var birthEra: Long = 0
  var deadEra: Option[Long] = None
  var trueEra: Option[Long] = None
  var killer: Option[String] = None

  def stalk(entityId: String): Unit = {
    //if (stalked.isEmpty) {
      val entity: Option[EntityLog] = consolidatedState.entityDynamicLog(entityId)
      if (entity.isDefined && entity.get.structuralData.reign == ReignType.ANIMAL.toString) {
        stalked = Some(entityId)
        birthEra = getBirthEra
        currentEra = birthEra
        deadEra = None
      }
    //}
  }

  def informAboutTrueEra(era: Long): Unit = {
    trueEra = Some(era)
    if (stalked.nonEmpty && killer.isEmpty) {
      consolidatedState.getAllDynamicLogs()
        .filter(x => x.structuralData.reign == ReignType.ANIMAL.toString).foreach(x => {
        x.dynamicData.foreach(y => y._2 match {
          case impl: AnimalDynamicDataImpl =>
            if (impl.eating.contains(stalked.get)) {
              deadEra = Some(y._1)
              killer = Some(x.id)
            }
        })
      })
    }
  }

  def unstalk: Unit = {
    stalked = None
  }

  def report: ResultEra = {

    var me: EntityTimedRecord = null
    var actions: Map[String, Seq[String]] = Map.empty
    var others:  Seq[ResultOther] = Seq(
      ResultOther("prey", Map.empty),
      ResultOther("partner", Map.empty),
      ResultOther("child", Map.empty),
      ResultOther("killer", Map.empty))

    if (stalked.isDefined) {

      if (deadEra.isEmpty && isStalkedDeadInThisEra) {
        deadEra = Some(currentEra)
        //println("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      }
      if ((deadEra.isDefined && currentEra >= deadEra.get) || (trueEra.isDefined && currentEra >= trueEra.get)) {
        currentEra = birthEra
      }


      var preys: Seq[String] = Seq.empty
      var partners: Seq[String] = Seq.empty
      var children: Seq[String] = Seq.empty
      getAllStalkedActions.foreach {
        case impl: AnimalDynamicDataImpl =>
          preys ++= impl.eating
          partners ++= impl.coupling
          children ++= impl.givingBirth
      }

      var allInteractionEntitiesId: Map[String, Seq[String]] = Map("prey" -> preys, "partner" -> partners, "child" -> children)

      if (killer.nonEmpty) {
        allInteractionEntitiesId += ("killer" -> Seq(killer.get))
      }

      allInteractionEntitiesId.foreach(tuple =>
        tuple._2.foreach(entity => if (consolidatedState.entitiesInEra(currentEra).exists(x => x.id == entity))
          others.filter(x => x.label == tuple._1).head.entities += (entity -> getPositionInThisEra(entity))))

      val target = consolidatedState.entitiesInEra(currentEra).filter(x => x.id == stalked.get) head

      target.dynamicData match {
        case impl: AnimalDynamicDataImpl =>
          actions += ("eat" -> impl.eating)
          actions += ("couple" -> impl.coupling)
          actions += ("give birth" -> impl.givingBirth)
      }

      me = target

      /*if (trueEra.isDefined)
        println((currentEra, trueEra.get))*/

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

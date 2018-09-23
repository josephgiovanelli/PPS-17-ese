package it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers

import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.dataminer.DataModelSupport.Era
import it.unibo.pps.ese.model.dataminer.datamodel._
import it.unibo.pps.ese.utils.Point

/**
  * Inform about what happend in the current era
  * @param me position of the stalked
  * @param others all the entities with whom the stalked interacted, who are alive in this era
  */
case class ResultEra(me: EntityTimedRecord, others: Seq[ResultOther])
case class ResultOther(label: String, var entities: Map[String, Point])

object MemoHelper {
  def memoize[I, O](f: I => O): I => O = new collection.mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }
}

case class Stalker(consolidatedState: ReadOnlyEntityRepository) {
  import MemoHelper._

  var stalked: Option[String] = None
  var currentEra: Long = 0
  var birthEra: Long = 0
  var deadEra: Option[Long] = None
  var trueEra: Option[Long] = None
  var killer: Option[String] = None
  lazy val memo: Long => ResultEra = memoize(x => {
    println(s"Calling memo with input $x")

    val others:  Seq[ResultOther] = Seq(
      ResultOther("prey", Map.empty),
      ResultOther("partner", Map.empty),
      ResultOther("child", Map.empty),
      ResultOther("killer", Map.empty))

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


    ResultEra(target, others)
  })

  def stalk(entityId: String): Unit = this synchronized {
    val entity: Option[EntityLog] = consolidatedState.entityDynamicLog(entityId)
    if (entity.isDefined && entity.get.structuralData.reign == ReignType.ANIMAL.toString) {
      stalked = Some(entityId)
      birthEra = getBirthEra
      currentEra = birthEra
      deadEra = None
    }
  }

  def informAboutTrueEra(era: Long): Unit = this synchronized {
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

  def unstalk: Unit = this synchronized {
    stalked = None
  }

  def report: ResultEra = this synchronized {

    var resultEra = ResultEra(null, null)

    if (stalked.isDefined) {

      if (deadEra.isEmpty && isStalkedDeadInThisEra) {
        deadEra = Some(currentEra)
      }
      if ((deadEra.isDefined && currentEra >= deadEra.get) || (trueEra.isDefined && currentEra >= trueEra.get)) {
        currentEra = birthEra
      }

      resultEra = memo(currentEra)

      if (killer.nonEmpty) {
        resultEra.others.filter(x => x.label == "killer").head.entities += (killer.get -> getPositionInThisEra(killer.get))
      }

      if (trueEra.isDefined && ((currentEra + 1) <= trueEra.get)) currentEra += 1

    }

    resultEra

  }

  private def getBirthEra: Era = {
    consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._1).min
  }

  private def isStalkedDeadInThisEra: Boolean = {
    !consolidatedState.entitiesInEra(currentEra).map(entity => entity.id).contains(stalked.get)
  }

  private def getAllStalkedActions: Seq[DynamicData] = {
    consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._2)
  }

  private def getPositionInThisEra(entity: String): Point = {
    consolidatedState.entitiesInEra(currentEra).filter(x => x.id == entity).head.dynamicData.position
  }
}

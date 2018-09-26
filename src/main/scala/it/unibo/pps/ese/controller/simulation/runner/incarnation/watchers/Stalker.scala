package it.unibo.pps.ese.controller.simulation.runner.incarnation.watchers

import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.dataminer.DataModelSupport.Era
import it.unibo.pps.ese.model.dataminer.datamodel._
import it.unibo.pps.ese.utils.Point

/**
  * Inform about what happens in a certain era
  * @param me all information about the stalked
  * @param others all the entities with whom the stalked interacted, who are alive in this era
  */
case class ResultEra(me: EntityTimedRecord, others: Seq[ResultOther])

/**
  * Group of entities with which the stalked has interacted in a certain era.
  * The entities are united by the interaction they had with the stalked
  * @param label kind of interaction
  * @param entities the entity identifier with related position
  */
case class ResultOther(label: String, var entities: Map[String, Point])

/**
  * Memoize pattern that allows caching to improve performance.
  */
object MemoHelper {
  def memoize[I, O](f: I => O): I => O = new collection.mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }
}

/**
  * Component that allows to reproduce the whole life of an entity from its birth, to its death, with all the positions it has visited and the entities with which it has interacted.
  * @param consolidatedState data structure that preserves all data regarding each era
  */
case class Stalker(consolidatedState: ReadOnlyEntityRepository) {
  import MemoHelper._

  /**
    * The identifier of the entity.
    */
  private var stalked: Option[String] = None

  /**
    * The variable that iterates all the era in which the stalkerate was alive.
    */
  var currentEra: Long = 0

  /**
    * The variable that is up to date with the real era in the simulation.
    */
  private var trueEra: Option[Long] = None

  /**
    * Information about the stalker life.
    */
  private var birthEra: Long = 0
  private var deadEra: Option[Long] = None
  private var killer: Option[String] = None

  /**
    * Function that calculate the result of each era and use the memoize to cache.
    */
  private lazy val memo: Long => ResultEra = memoize(x => {
    println(s"Calling memo with input $x")

    //the variables are initialized
    val others:  Seq[ResultOther] = Seq(
      ResultOther("prey", Map.empty),
      ResultOther("partner", Map.empty),
      ResultOther("child", Map.empty),
      ResultOther("killer", Map.empty))
    var preys: Seq[String] = Seq.empty
    var partners: Seq[String] = Seq.empty
    var children: Seq[String] = Seq.empty

    //get the actions in the all life
    getAllStalkedActions.foreach {
      case impl: AnimalDynamicDataImpl =>
        preys ++= impl.eating
        partners ++= impl.coupling
        children ++= impl.givingBirth
    }
    val allInteractionEntitiesId: Map[String, Seq[String]] =
      Map("prey" -> preys, "partner" -> partners, "child" -> children)

    //get the position of the entities that have interaction in all life of the stalkered, and are alive in the current era
    allInteractionEntitiesId.foreach(tuple =>
      tuple._2.foreach(entity => if (consolidatedState.entitiesInEra(currentEra).exists(x => x.id == entity))
        others.filter(x => x.label == tuple._1).head.entities += (entity -> getPositionInThisEra(entity).get)))

    //get information about the stalked in the current era
    val target = consolidatedState.entitiesInEra(currentEra).filter(x => x.id == stalked.get) head

    ResultEra(target, others)
  })

  /**
    * It allows to set the entity to watch.
    * @param entityId entity identifier
    */
  def stalk(entityId: String): Unit = this synchronized {
    val entity: Option[EntityLog] = consolidatedState.entityDynamicLog(entityId)
    if (entity.isDefined && entity.get.structuralData.reign == ReignType.ANIMAL.toString) {
      stalked = Some(entityId)
      birthEra = getBirthEra
      currentEra = birthEra
      deadEra = None
    }
  }

  /**
    * It allows to update the replay with the next true era, when it is ready.
    * @param era the real era in the simulation
    */
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

  /**
    * It allows to unwatch the stalked entity.
    */
  def unstalk(): Unit = this synchronized {
    stalked = None
  }

  /**
    * This is the method that report you the result of the current era and increment the current era in order to be ready for the next iteration.
    * @return the information about the stalked and the entities that interacted with him
    */
  def report: ResultEra = this synchronized {

    var resultEra = ResultEra(null, null)

    //if the stalked is defined
    if (stalked.isDefined) {

      //calculate the dead era
      if (deadEra.isEmpty && isStalkedDeadInThisEra) {
        deadEra = Some(currentEra)
      }
      //and if the current era has reached the true era, the replay starts again
      if ((deadEra.isDefined && currentEra >= deadEra.get) || (trueEra.isDefined && currentEra >= trueEra.get)) {
        currentEra = birthEra
      }

      //if the result era of the current era is not cached, this is calculated
      resultEra = memo(currentEra)

      //calculate the position of the killer if is present
      if (killer.nonEmpty) {
        val killerPosition: Option[Point] = getPositionInThisEra(killer.get)
        if (killerPosition.isDefined) {
          resultEra.others.filter(x => x.label == "killer").head.entities += (killer.get -> killerPosition.get)
        }
      }

      //increment the current era
      if (trueEra.isDefined && ((currentEra + 1) <= trueEra.get)) currentEra += 1

    }

    resultEra

  }

  /**
    * Calculate the birth era of the stalked.
    * @return the birth era
    */
  private def getBirthEra: Era = {
    consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._1).min
  }

  /**
    * Check if the stalked is alive in the current era.
    * @return if the stalked is alive
    */
  private def isStalkedDeadInThisEra: Boolean = {
    !consolidatedState.entitiesInEra(currentEra).map(entity => entity.id).contains(stalked.get)
  }

  /**
    * Get all the actions about the all life of the stalked.
    * @return the actions
    */
  private def getAllStalkedActions: Seq[DynamicData] = {
    consolidatedState.getAllDynamicLogs().filter(x => x.id == stalked.get).flatMap(x => x.dynamicData).map(x => x._2)
  }

  /**
    * If is present, get the position of the entity in the current era.
    * @param entity the entity identifier
    * @return the position
    */
  private def getPositionInThisEra(entity: String): Option[Point] = {
    val entityLog: Option[EntityLog] = consolidatedState.entityDynamicLog(entity)
    val entityLogInCurrentEra: Seq[(Era, DynamicData)] =
      if (entityLog.isDefined) entityLog.get.dynamicData.filter(era => era._1.equals(currentEra)) else Seq.empty
    if (entityLogInCurrentEra.nonEmpty) {
      Some(entityLogInCurrentEra.head._2.position)
    } else {
      None
    }

  }
}

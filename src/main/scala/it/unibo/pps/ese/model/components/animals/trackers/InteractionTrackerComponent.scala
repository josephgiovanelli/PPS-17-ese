package it.unibo.pps.ese.model.components.animals.trackers

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.BaseEvent
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.model.components.animals.brain.{BrainComponent, Couple, Eat, InteractionEntity}

import scala.concurrent.ExecutionContext

/**
  * Event with which inform the rest of the world of [[InteractionTrackerComponent]] state.
  * @param eat the entities eaten in this iteration
  * @param couple the entities with which coupled in this iteration
  * @param create the children born in this iteration
  */
case class InteractionTrackerInfo(eat: Seq[String],
                             couple: Seq[String],
                             create: Seq[String]) extends BaseEvent

/**
  * It clears all interaction for the next iteration.
  */
case class ClearInteraction() extends BaseEvent

/**
  * It informs that a meal is happened.
  */
case class EatInteraction() extends BaseEvent

/**
  * It informs that a couple is happened
  */
case class CoupleInteraction() extends BaseEvent

/**
  * It informs that a birth is happened.
  */
case class CreateInteraction() extends BaseEvent

/**
  * This component tracks all interactions of the entity, in each iteration.
  * @param entitySpecifications the base information of the entity
  * @param executionContext the execution context
  */
class InteractionTrackerComponent(override val entitySpecifications: EntitySpecifications)
                                 (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications)  {

  /**
    * The identifiers of the entities with whom you have interacted.
    */
  var eat: Seq[String] = Seq.empty
  var couple: Seq[String] = Seq.empty
  var create: Seq[String] = Seq.empty

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  /**
    * Method by which events are received and the brain specifies the reaction to them.
    */
  private def subscribeEvents(): Unit = {
    subscribe {
      /*
      In each iteration all interactions are cleared.
       */
      case ComputeNextState() =>
        eat = Seq.empty
        couple = Seq.empty
        create = Seq.empty
        publish(ClearInteraction())
        publish(new ComputeNextStateAck)
      /*
      In case of interaction the internal fields are updated and the state is communicate.
       */
      case InteractionEntity(id, action) => action match {
        case Eat =>
          eat = Seq(id)
          publish(EatInteraction())
        case Couple =>
          couple = Seq(id)
          publish(CoupleInteraction())
        case _ =>
      }
      /*
      In this case also a born is an interaction.
       */
      case GiveBirth(sons) =>
        create = sons
        publish(CreateInteraction())
      /*
      If this message is received the brain has to communicate his information.
       */
      case GetInfo() =>
        publish(InteractionTrackerInfo(eat, couple, create))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  /**
    * The exterior can see the component as a set of attributes.
    * These are modified by the published events.
    * This method defines how events change attributes.
    */
  private def configureMappings(): Unit = {
    addMapping[InteractionTrackerInfo]((classOf[InteractionTrackerInfo], ev => Seq(
      EntityProperty("eat", ev eat),
      EntityProperty("couple", ev couple),
      EntityProperty("create", ev create),
      EntityProperty("genes", Seq.empty)
    )))

    addMapping[ClearInteraction]((classOf[ClearInteraction], _ => Seq(
      EntityProperty("eat", eat),
      EntityProperty("couple", couple),
      EntityProperty("create", create)
    )))

    addMapping[EatInteraction]((classOf[EatInteraction], _ => Seq(
      EntityProperty("eat", eat)
    )))

    addMapping[CoupleInteraction]((classOf[CoupleInteraction], _ => Seq(
      EntityProperty("couple", couple)
    )))

    addMapping[CreateInteraction]((classOf[CreateInteraction], _ => Seq(
      EntityProperty("create", create)
    )))
  }

}

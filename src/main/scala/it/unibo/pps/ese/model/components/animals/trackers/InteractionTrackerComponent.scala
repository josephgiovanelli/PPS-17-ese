package it.unibo.pps.ese.model.components.animals.trackers

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.BaseEvent
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.model.components.animals.brain.{Couple, Eat, InteractionEntity}

import scala.concurrent.ExecutionContext

case class InteractionTrackerInfo(eat: Seq[String],
                             couple: Seq[String],
                             create: Seq[String]) extends BaseEvent

case class ClearInteraction() extends BaseEvent
case class EatInteraction() extends BaseEvent
case class CoupleInteraction() extends BaseEvent
case class CreateInteraction() extends BaseEvent

class InteractionTrackerComponent(override val entitySpecifications: EntitySpecifications)
                                 (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications)  {

  var eat: Seq[String] = Seq.empty
  var couple: Seq[String] = Seq.empty
  var create: Seq[String] = Seq.empty

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  private def subscribeEvents(): Unit = {
    subscribe {
      case ComputeNextState() =>
        eat = Seq.empty
        couple = Seq.empty
        create = Seq.empty
        publish(ClearInteraction())
        publish(new ComputeNextStateAck)
      case InteractionEntity(id, action) => action match {
        case Eat =>
          eat = Seq(id)
          publish(EatInteraction())
        case Couple =>
          couple = Seq(id)
          publish(CoupleInteraction())
        case _ =>
      }
      case GiveBirth(sons) =>
        create = sons
        publish(CreateInteraction())
      case GetInfo() =>
        publish(InteractionTrackerInfo(eat, couple, create))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  private def configureMappings(): Unit = {
    addMapping[InteractionTrackerInfo]((classOf[InteractionTrackerInfo], ev => Seq(
      EntityProperty("eat", ev eat),
      EntityProperty("couple", ev couple),
      EntityProperty("create", ev create)
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

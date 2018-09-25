package it.unibo.pps.ese.controller.simulation.runner.core

import java.util.UUID.randomUUID

/**
  * This package object contains the base traits and types recognized by the EventBus
  */
package object EventBusSupport {

  /**
    * A class must extend this treat to be sent on the EventBus
    */
  trait BaseEvent

  /**
    * Extending this trait, a class is served with priority when sent on the bus
    */
  trait HighPriorityEvent

  /**
    * A class extending this trait represents a data request
    */
  trait RequestEvent extends BaseEvent { val id : String = randomUUID().toString }

  /**
    * A class extending this trait represents a data response
    */
  trait ResponseEvent extends BaseEvent { val id : String }

  /**
    * A class extending this trait represents a message for another bus
    */
  trait InteractionEvent extends BaseEvent { val receiverId: String }

  /**
    * The envelope used to deliver Interaction events
    * @param sourceId The sender identifier
    * @param targetId The receiver identifier
    * @param message The message to be sent
    * @tparam A The message type
    */
  case class InteractionEnvelope[A <: InteractionEvent](sourceId : String, targetId: String, message: A)

  /**
    * Represents an OK response
    */
  class Done

  /**
    * Events on the bus are decorated with this wrapper to track who sent the message
    * @param sourceId The sender identifier
    * @param event The message to be sent
    */
  case class IdentifiedEvent(sourceId: String, event: Event)

  /**
    * Bus consumers are decorated with this wrapper to track who registered them
    * @param sourceId Who registers the consumer
    * @param consumer The consumer
    */
  case class IdentifiedConsumer(sourceId: String, consumer: Event => Unit)

  type Consumer = IdentifiedConsumer
  type RequestConsumer = RequestEvent => Unit
  type Event = BaseEvent

}

package it.unibo.pps.ese.genericworld.model

import java.util.UUID.randomUUID

package object support {

  trait BaseEvent
  trait HighPriorityEvent
  trait RequestEvent extends BaseEvent { val id : String = randomUUID().toString }
  trait ResponseEvent extends BaseEvent { val id : String }
  trait InteractionEvent extends BaseEvent { val receiverId: String }

  case class InteractionEnvelope[A](sourceId : String, targetId: String, message: A)

  @SerialVersionUID(100L)
  class Done extends Serializable

  case class IdentifiedEvent(sourceId: String, event: Event)
  case class IdentifiedConsumer(sourceId: String, consumer: Event => Unit)

  type Consumer = IdentifiedConsumer
  type RequestConsumer = RequestEvent => Unit
  type Event = BaseEvent

}

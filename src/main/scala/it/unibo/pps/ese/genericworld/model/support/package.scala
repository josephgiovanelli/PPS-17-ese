package it.unibo.pps.ese.genericworld.model

import java.util.UUID.randomUUID

package object support {

  trait BaseEvent
  trait RequestEvent extends BaseEvent { val id : String = randomUUID().toString }
  trait ResponseEvent extends BaseEvent { val id : String }
  trait InteractionEvent extends BaseEvent { val receiverId: String }

  case class InteractionEnvelope[A](sourceId : String, targetId: String, message: A)
  class Done

  type Consumer = Event => Unit
  type RequestConsumer = RequestEvent => Unit
  type Event = BaseEvent

}

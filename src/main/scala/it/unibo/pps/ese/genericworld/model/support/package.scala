package it.unibo.pps.ese.genericworld.model

import java.util.UUID.randomUUID

package object support {

  class BaseEvent
  class RequestEvent(identifier : String = randomUUID().toString) extends BaseEvent { val id : String = identifier }
  class ResponseEvent(identifier : String) extends BaseEvent { val id : String = identifier }
  class Done

  type Consumer = Event => Unit
  type RequestConsumer = RequestEvent => Unit
  type Event = BaseEvent

}

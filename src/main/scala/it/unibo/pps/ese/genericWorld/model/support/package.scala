package it.unibo.pps.ese.genericWorld.model

package object support {

  class BaseEvent
  class Done

  type Consumer = Event => Unit
  type Event = BaseEvent

}

package it.unibo.pps.ese.genericworld.model

package object support {

  class BaseEvent
  class Done

  type Consumer = Event => Unit
  type Event = BaseEvent

}

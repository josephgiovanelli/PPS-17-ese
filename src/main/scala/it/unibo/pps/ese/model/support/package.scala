package it.unibo.pps.ese.model

package object support {

  class BaseEvent

  type Consumer = Event => Unit
  type Event = BaseEvent
}

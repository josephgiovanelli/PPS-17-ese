package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.{Consumer, Event, EventBus}

trait Component {
  def initialize(): Unit
}

trait NervousSystemComponent extends Component {
  private[this] var _nervousSystem : Option[EventBus] = None
  def nervousSystem : EventBus = _nervousSystem getOrElse(throw new RuntimeException("Error"))
  def nervousSystem_=(eventBus: EventBus) : Unit = _nervousSystem = Some(eventBus)
}

trait BusWriter extends NervousSystemComponent {
  def writeData(event : Event) : Unit = nervousSystem send event
}

trait BusReader extends NervousSystemComponent {
  def subscribe(consumer : Consumer) : Unit = nervousSystem attach consumer
}

trait ComponentLocator extends Component {
  def findComponent(component: Component ) : Option[Component] = None
}

abstract class BaseReaderComponent extends Component with ComponentLocator with BusReader
abstract class BaseWriterComponent extends BaseReaderComponent with BusWriter

abstract class ReaderComponent extends BaseReaderComponent
abstract class WriterComponent extends BaseWriterComponent

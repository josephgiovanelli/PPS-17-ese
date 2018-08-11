package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.{Consumer, Event, EventBus}

import scala.reflect.ClassTag

trait Component {
  def initialize(): Unit
  def entityId : String
}

trait NervousSystemComponent extends Component {
  private[this] var _nervousSystem : Option[NervousSystem] = None
  def nervousSystem : NervousSystem = _nervousSystem getOrElse(throw new RuntimeException("Error"))
  def nervousSystem_=(nervousSystem: NervousSystem) : Unit = _nervousSystem = Some(nervousSystem)
}

trait BusWriter extends NervousSystemComponent {
  def publish(event : Event): Unit = nervousSystem publish event
  def addMapping[A <: Event](mapper: (Class[A] ,A => Seq[EntityProperty])): Unit = nervousSystem addMapping mapper
}

trait BusReader extends NervousSystemComponent {
  def subscribe(consumer : Consumer) : Unit = nervousSystem subscribe consumer
}

abstract class BaseComponent(override val entityId: String) extends Component

abstract class BaseReaderComponent(entityId: String) extends BaseComponent(entityId)  with BusReader
abstract class BaseWriterComponent(entityId: String) extends BaseReaderComponent(entityId) with BusWriter

abstract class ReaderComponent(entityId: String) extends BaseReaderComponent(entityId)
abstract class WriterComponent(entityId: String) extends BaseWriterComponent(entityId)

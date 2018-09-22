package it.unibo.pps.ese.controller.simulation.runner.core

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.controller.simulation.runner.core.support._

import scala.concurrent.{ExecutionContext, Future}

trait Component {
  def initialize(): Unit
  def entitySpecifications : EntitySpecifications
}

trait NervousSystemComponent extends Component {
  implicit def executionContext: ExecutionContext
  private[this] var _nervousSystem : Option[NervousSystem] = None
  def nervousSystem : NervousSystem = _nervousSystem getOrElse(throw new RuntimeException("Error"))
  def nervousSystem_=(nervousSystem: NervousSystem) : Unit = _nervousSystem = Some(nervousSystem)
}

trait BusWriter extends NervousSystemComponent {
  def publish(event : Event): Unit =
    nervousSystem publish IdentifiedEvent(entitySpecifications.id + "#" + getClass.getSimpleName, event)
  def addMapping[A <: Event](mapper: (Class[A] ,A => Seq[EntityProperty])): Unit = nervousSystem addMapping mapper
}

trait BusReader extends NervousSystemComponent {
  def subscribe(consumer : Event => Unit) : Unit =
    nervousSystem subscribe IdentifiedConsumer(entitySpecifications.id + "#" + getClass.getSimpleName, consumer)
  def requireData[A <: RequestEvent, B <: ResponseEvent : Manifest](request: A): SupervisedFuture[B] =
    nervousSystem requireData[A, B] (entitySpecifications.id + "#" + getClass.getSimpleName, request)
  def notifyOnTasksEnd(): Future[Done] = nervousSystem notifyOnTasksEnd()
}

abstract class BaseComponent(override val entitySpecifications: EntitySpecifications) extends Component

abstract class BaseReaderComponent(entitySpecifications: EntitySpecifications)
  extends BaseComponent(entitySpecifications)  with BusReader
abstract class BaseWriterComponent(entitySpecifications: EntitySpecifications)
  extends BaseReaderComponent(entitySpecifications) with BusWriter

abstract class ReaderComponent(entitySpecifications: EntitySpecifications)
  extends BaseReaderComponent(entitySpecifications)
abstract class WriterComponent(entitySpecifications: EntitySpecifications)
  extends BaseWriterComponent(entitySpecifications)

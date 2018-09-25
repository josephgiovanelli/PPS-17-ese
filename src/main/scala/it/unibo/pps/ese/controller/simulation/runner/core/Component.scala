package it.unibo.pps.ese.controller.simulation.runner.core

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityProperty
import it.unibo.pps.ese.controller.simulation.runner.core.support._

import scala.concurrent.{ExecutionContext, Future}

/**
  * Trait containing the base APIs of a component. Components represent the building block of witch entities
  * are made. Extending them it's possible to mold the dynamic behaviour of an entity, maintaining a common layer
  * that takes care of  basic features like communication and data collection
  */
trait Component {

  /**
    * Initialize the component
    */
  def initialize(): Unit

  /**
    * Get the associated entity's info
    * @return Entity's info
    */
  def entitySpecifications : EntitySpecifications
}

/**
  * Extension of the base Component that adds the NervousSystem capabilities
  */
trait NervousSystemComponent extends Component {
  implicit def executionContext: ExecutionContext
  private[this] var _nervousSystem : Option[NervousSystem] = None

  /**
    * Get the Nervous System
    * @return The associated Nervous System
    */
  private[core] def nervousSystem : NervousSystem = _nervousSystem getOrElse(throw new RuntimeException("Error"))

  /**
    * Set the Nervous System
    * @param nervousSystem The NervousSystem to associate
    */
  private[core] def nervousSystem_=(nervousSystem: NervousSystem) : Unit = _nervousSystem = Some(nervousSystem)
}

/**
  * Enables a component to the read only operations on the NervousSystem
  */
trait BusWriter extends NervousSystemComponent {
  protected def publish(event : Event): Unit =
    nervousSystem publish IdentifiedEvent(entitySpecifications.id + "#" + getClass.getSimpleName, event)
  protected def addMapping[A <: Event](mapper: (Class[A] ,A => Seq[EntityProperty])): Unit =
    nervousSystem addMapping mapper
}

/**
  * Enables a component to the write operations on the NervousSystem
  */
trait BusReader extends NervousSystemComponent {
  protected def subscribe(consumer : Event => Unit) : Unit =
    nervousSystem subscribe IdentifiedConsumer(entitySpecifications.id + "#" + getClass.getSimpleName, consumer)
  protected def requireData[A <: RequestEvent, B <: ResponseEvent : Manifest](request: A): SupervisedFuture[B] =
    nervousSystem requireData[A, B] (entitySpecifications.id + "#" + getClass.getSimpleName, request)
  protected def notifyOnTasksEnd(): Future[Done] = nervousSystem notifyOnTasksEnd()
}

/**
  * Abstract base component
  * @param entitySpecifications Belonging entity's info
  */
abstract class BaseComponent(override val entitySpecifications: EntitySpecifications) extends Component

/**
  * Abstract NervousSystem's reader component
  * @param entitySpecifications Belonging entity's info
  */
abstract class ReaderComponent(entitySpecifications: EntitySpecifications)
  extends BaseComponent(entitySpecifications) with BusReader

/**
  * Abstract NervousSystem's writer component
  * @param entitySpecifications Belonging entity's info
  */
abstract class WriterComponent(entitySpecifications: EntitySpecifications)
  extends ReaderComponent(entitySpecifications) with BusWriter

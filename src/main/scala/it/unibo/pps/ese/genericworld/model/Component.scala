package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.controller.saving.{Memento, Savable}
import it.unibo.pps.ese.entitybehaviors.{AbstractForceReproductionMemento, BrainComponent, ForceReproduction}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.Hippocampus.HippocampusMemento
import it.unibo.pps.ese.entitybehaviors.decisionsupport.EntityAttributesImpl.EntityAttributesImpl
import it.unibo.pps.ese.genericworld.model.Entity.AbstractEntityMemento
import it.unibo.pps.ese.genericworld.model.support._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genetics.dna.AnimalGenome

import scala.concurrent.{ExecutionContext, Future}

trait Component extends Savable[AbstractComponentMemento] {
  def initialize(): Unit
  def entitySpecifications : EntitySpecifications
}

object Component {
  def apply(abstractComponentMemento: AbstractComponentMemento)(implicit executionContext: ExecutionContext): Component =
    abstractComponentMemento match {
    case b: BrainComponentMemento => BrainComponent(b)
  }
}

trait NervousSystemComponent extends Component {
  implicit def executionContext: ExecutionContext
  private[this] var _nervousSystem : Option[NervousSystem] = None
  def nervousSystem : NervousSystem = _nervousSystem getOrElse(throw new RuntimeException("Error"))
  def nervousSystem_=(nervousSystem: NervousSystem) : Unit = _nervousSystem = Some(nervousSystem)
}

trait BusWriter extends NervousSystemComponent {
  def publish(event : Event): Unit = nervousSystem publish IdentifiedEvent(entitySpecifications.id + "#" + getClass.getSimpleName, event)
  def addMapping[A <: Event](mapper: (Class[A] ,A => Seq[EntityProperty])): Unit = nervousSystem addMapping mapper
}

trait BusReader extends NervousSystemComponent {
  def subscribe(consumer : Event => Unit) : Unit = nervousSystem subscribe IdentifiedConsumer(entitySpecifications.id + "#" + getClass.getSimpleName, consumer)
  def requireData[A <: RequestEvent, B <: ResponseEvent : Manifest](request: A): SupervisedFuture[B] = nervousSystem requireData[A, B] (entitySpecifications.id + "#" + getClass.getSimpleName, request)
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

sealed abstract class AbstractComponentMemento extends Memento

case class BrainComponentMemento(abstractEntityMemento: AbstractEntityMemento,
                                 heightWorld: Int,
                                 widthWorld: Int,
                                 strong: Double,
                                 actionField: Double,
                                 visualField: Double,
                                 attractiveness: Double,
                                 digestionState: Boolean,
                                 forceReproductionMemento: Option[AbstractForceReproductionMemento],
                                 hippocampusMemento: HippocampusMemento,
                                 entityInVisualField: Map[String, EntityAttributesImpl]) extends AbstractComponentMemento

case class ReprodcutionComponentMemento(abstractEntityMemento: AbstractEntityMemento,
                                        fecundity: Double,
                                        geneticsSimulator: GeneticsSimulator,
                                        animalGenome: AnimalGenome,
                                        pregnancyDuration: Double,
                                        clocksPerYear: Long,
                                        mutationProb: Double,
                                        energyRequirements: Double) extends AbstractComponentMemento


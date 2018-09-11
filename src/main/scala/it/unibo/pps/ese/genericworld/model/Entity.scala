package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.controller.saving.{Memento, Savable}
import it.unibo.pps.ese.genericworld.model.Entity.AbstractEntityMemento

import scala.concurrent.ExecutionContext


sealed trait Entity extends Savable[AbstractEntityMemento] {
  def id: String
  def getComponents : Seq[Component]
  def addComponent(component : Component) : Unit
  def specifications : EntitySpecifications
  def dispose(): Unit
}

sealed trait EntitySpecifications extends Savable[AbstractEntityMemento] {
  def id: String
  def componentsCount : Long
}

sealed trait NervousSystemExtension {
  implicit def executionContext: ExecutionContext
  private[this] val _nervousSystem = NervousSystem()
  def nervousSystem : ManageableNervousSystem = _nervousSystem
}

object Entity {

  def apply(instance: String, id: String)(implicit executionContext: ExecutionContext): Entity = instance match {
    case "base" => new BaseEntity(id, Seq.empty)
    case "improved" => new ImprovedEntity(id, Seq.empty) with NervousSystemExtension
  }

  def apply(abstractEntityMemento: AbstractEntityMemento)(implicit executionContext: ExecutionContext): EntitySpecifications = {
    abstractEntityMemento match {
      case m: BaseEntityMemento => new BaseEntity(m.entityId, m.components.map(m => Component(m)))
      case m: ImprovedEntityMemento => new ImprovedEntity(m.id, m.components.map(m => Component(m))) with NervousSystemExtension
    }
  }

  private abstract class AbstractBaseEntity(entityId: String, private[this] var components: Seq[Component])
    extends Entity with EntitySpecifications {

    override def id: String = entityId

    override def getComponents: Seq[Component] = components

    override def addComponent(component: Component): Unit = components = components :+ component

    override def componentsCount: Long = getComponents size

    override def specifications: EntitySpecifications = this

    override def dispose(): Unit = Unit
  }

  private class BaseEntity(entityId: String,
                           components: Seq[Component])
                          (implicit val executionContext: ExecutionContext)
    extends AbstractBaseEntity(entityId, components) {

    override def serialize: BaseEntityMemento = {
      BaseEntityMemento(entityId, getComponents.map(c => c.serialize))
    }
  }

  private class ImprovedEntity(id: String,
                               components: Seq[Component])
                              (implicit val executionContext: ExecutionContext)
    extends AbstractBaseEntity(id, components) {

    self : NervousSystemExtension =>
    override def addComponent(component: Component): Unit = {
      component match {
        case c: NervousSystemComponent => c nervousSystem_= nervousSystem
        case _ => Unit
      }
      super.addComponent(component)
      component initialize()
    }

    override def dispose(): Unit = nervousSystem dispose()

    override def serialize: ImprovedEntityMemento = {
      ImprovedEntityMemento(id, getComponents.map(c => c.serialize))
    }
  }

  sealed abstract class AbstractEntityMemento extends Memento

  case class BaseEntityMemento(entityId: String, components : Seq[AbstractComponentMemento]) extends AbstractEntityMemento

  case class ImprovedEntityMemento(id: String, components : Seq[AbstractComponentMemento]) extends AbstractEntityMemento
}

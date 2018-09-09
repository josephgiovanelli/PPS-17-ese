package it.unibo.pps.ese.genericworld.model

import it.unibo.pps.ese.controller.saving.Savable
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
    case "base" => new BaseEntity(id)
    case "improved" => new ImprovedEntity(id) with NervousSystemExtension
  }

  def apply(abstractEntityMemento: AbstractEntityMemento)(implicit executionContext: ExecutionContext): EntitySpecifications = {
    abstractEntityMemento match {
      case m: BaseEntityMemento => new BaseEntity(m)
      case m: ImprovedEntityMemento => new ImprovedEntity(m) with NervousSystemExtension
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

  private class BaseEntity(entityId: String, components: Seq[Component] = Seq.empty)
    extends AbstractBaseEntity(entityId, components) {

    def this(baseEntityMemento: BaseEntityMemento)(implicit executionContext: ExecutionContext) {
      this(baseEntityMemento.entityId, baseEntityMemento.components)
    }

    override def serialize: BaseEntityMemento = {
      BaseEntityMemento(entityId, getComponents)
    }
  }

  private class ImprovedEntity(id: String, components: Seq[Component] = Seq.empty)(implicit val executionContext: ExecutionContext)
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

    def this(improvedEntityMemento: ImprovedEntityMemento)(implicit executionContext: ExecutionContext) {
      this(improvedEntityMemento.id, improvedEntityMemento.components)
    }

    override def dispose(): Unit = nervousSystem dispose()

    override def serialize: ImprovedEntityMemento = {
      ImprovedEntityMemento(id, getComponents)
    }
  }

  sealed abstract class AbstractEntityMemento

  case class BaseEntityMemento(entityId: String, components : Seq[Component]) extends AbstractEntityMemento

  case class ImprovedEntityMemento(id: String, components : Seq[Component]) extends AbstractEntityMemento
}

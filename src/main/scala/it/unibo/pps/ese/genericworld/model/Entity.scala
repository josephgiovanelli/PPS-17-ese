package it.unibo.pps.ese.genericworld.model

import scala.concurrent.ExecutionContext

sealed trait Entity {
  def id: String
  def getComponents : Seq[Component]
  def addComponent(component : Component) : Unit
  def specifications : EntitySpecifications
  def dispose(): Unit
}

sealed trait EntitySpecifications {
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

  private class BaseEntity(entityId: String) extends Entity with EntitySpecifications {
    private[this] var components : Seq[Component] = Seq.empty

    override def id: String = entityId

    override def getComponents: Seq[Component] = components

    override def addComponent(component: Component): Unit = components = components :+ component

    override def componentsCount: Long = getComponents size

    override def specifications: EntitySpecifications = this

    override def dispose(): Unit = Unit
  }

  private class ImprovedEntity(id: String)(implicit val executionContext: ExecutionContext) extends BaseEntity(id) {

    self : NervousSystemExtension =>
    override def addComponent(component: Component): Unit = {
      component match {
        case c : NervousSystemComponent => c nervousSystem_= nervousSystem
      }
      super.addComponent(component)
      component initialize()
    }

    override def dispose(): Unit = nervousSystem dispose()
  }
}

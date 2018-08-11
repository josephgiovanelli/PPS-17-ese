package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.{Event, EventBus}

sealed trait Entity {
  def id: String
  def getComponents : Seq[Component]
  def addComponent(component : Component) : Unit
}

sealed trait NervousSystemExtension {
  private[this] val _nervousSystem = NervousSystem()
  def nervousSystem : NervousSystem = _nervousSystem
}

object Entity {

  def apply(instance: String, id: String): Entity = instance match {
    case "base" => new BaseEntity(id)
    case "improved" => new ImprovedEntity(id) with NervousSystemExtension
  }

  private class BaseEntity(entityId: String) extends Entity {
    private[this] var components : Seq[Component] = Seq.empty

    override def id: String = entityId

    override def getComponents: Seq[Component] = components

    override def addComponent(component: Component): Unit = components = components :+ component
  }

  private class ImprovedEntity(id: String) extends BaseEntity(id) {

    self : NervousSystemExtension =>
    override def addComponent(component: Component): Unit = {
      component match {
        case c : NervousSystemComponent => c nervousSystem_= nervousSystem
      }
      super.addComponent(component)
      component initialize()
    }
  }

}

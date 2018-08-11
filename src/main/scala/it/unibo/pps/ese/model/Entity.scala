package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.EventBus

sealed trait NervousSystem {
  def eventBus : EventBus
}

trait BaseNervousSystem extends NervousSystem {
  private[this] val _eventBus = EventBus()
  override def eventBus : EventBus = _eventBus
}

sealed trait Entity {
  def id: String
  def getComponents : Seq[Component]
  def addComponent(component : Component) : Unit
}

object Entity {

  def apply(instance: String, id: String): Entity = instance match {
    case "improved" => new ImprovedEntity(id) with BaseNervousSystem
  }

  private abstract case class BaseEntity(id: String) extends Entity {

    private[this] var components : Seq[Component] = Seq.empty

    override def getComponents: Seq[Component] = components
    override def addComponent(component: Component): Unit = components = components :+ component
  }

  private class ImprovedEntity(override val id: String) extends BaseEntity(id) {

    self : NervousSystem =>
    override def addComponent(component: Component): Unit = {
      component match {
        case c : NervousSystemComponent => c nervousSystem_= eventBus
      }
      super.addComponent(component)
      component initialize
    }
  }

}

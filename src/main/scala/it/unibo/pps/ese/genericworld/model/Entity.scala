package it.unibo.pps.ese.genericworld.model

sealed trait Entity {
  def id: String
  def getComponents : Seq[Component]
  def addComponent(component : Component) : Unit
  def specifications : EntitySpecifications
}

sealed trait EntitySpecifications {
  def id: String
  def componentsCount : Long
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

  private class BaseEntity(entityId: String) extends Entity with EntitySpecifications {
    private[this] var components : Seq[Component] = Seq.empty

    override def id: String = entityId

    override def getComponents: Seq[Component] = components

    override def addComponent(component: Component): Unit = components = components :+ component

    override def componentsCount: Long = getComponents size

    override def specifications: EntitySpecifications = this
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

package it.unibo.pps.ese.controller.simulation.runner.core

import scala.concurrent.ExecutionContext

/**
  * Trait that defines the operations available on simulation members
  */
sealed trait Entity {
  /**
    * Get the entity identifier
    * @return
    */
  def id: String

  /**
    * Get a list of the components registered on this entity
    * @return
    */
  def getComponents : Seq[Component]

  /**
    * Register a new component on this entity
    * @param component The component to register
    */
  def addComponent(component : Component) : Unit

  /**
    * Get info about this entity
    * @return Entity's info
    */
  def specifications : EntitySpecifications

  /**
    * Require the disposal of the entity resources
    */
  def dispose(): Unit
}

/**
  * This trait defines the public info of an entity, made available after a specifications request.
  */
sealed trait EntitySpecifications {

  /**
    * @return The entity identifier
    */
  def id: String

  /**
    * The number of components registered on this entity
    * @return
    */
  def componentsCount : Long
}

/**
  * This trait, if added to an entity, enables communication between components
  */
sealed trait NervousSystemExtension {

  /**
    * An execution context is requested for execution
    * @return
    */
  implicit def executionContext: ExecutionContext

  private[this] val _nervousSystem = NervousSystem()

  /**
    * Returns the instance of the nervous system associated to the entity
    * @return
    */
  def nervousSystem : ManageableNervousSystem = _nervousSystem
}

object Entity {

  /**
    * @param id The entity identifier
    * @param executionContext An execution context, necessary for async tasks
    * @return An Entity instance
    */
  def apply(id: String)(implicit executionContext: ExecutionContext): Entity =
    new ImprovedEntity(id) with NervousSystemExtension

  /**
    * Base implementation of Entity trait. There's no NervousSystem associated, so components are not communication
    * enabled. The EntitySpecification trait is directly mixed in the class so the specifications call
    * returns an instance of the entity itself under a different interface
    * @param entityId The entity identifier
    */
  private class BaseEntity(entityId: String) extends Entity with EntitySpecifications {
    private[this] var components : Seq[Component] = Seq.empty

    override def id: String = entityId

    override def getComponents: Seq[Component] = components

    override def addComponent(component: Component): Unit = components = components :+ component

    override def componentsCount: Long = getComponents size

    override def specifications: EntitySpecifications = this

    override def dispose(): Unit = Unit
  }

  /**
    * This improved version requires the mix-in of a NervousSystemExtension. The registered components are connected
    * through it.
    * @param id The entity identifier
    * @param executionContext An execution context, necessary for async tasks
    */
  private class ImprovedEntity(id: String)(implicit val executionContext: ExecutionContext) extends BaseEntity(id) {

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
  }
}

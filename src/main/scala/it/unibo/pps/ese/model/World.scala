package it.unibo.pps.ese.model

sealed trait World {
  def getEntities : Seq[Entity]
  def addEntity(entity : Entity) : Unit
}

object World {

  def apply(): World = new BaseWorld

  private case class BaseWorld() extends World {
    private[this] var entities : Seq[Entity] = Seq.empty

    override def getEntities: Seq[Entity] = entities
    override def addEntity(entity: Entity): Unit = entities = entities :+ entity
  }
}

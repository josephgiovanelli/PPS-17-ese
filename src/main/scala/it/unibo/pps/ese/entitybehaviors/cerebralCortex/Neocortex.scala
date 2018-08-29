package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.view.Position


trait Neocortex {
  def addMemory(memoryType: MemoryType, memory: Memory)
  def getMemeory(memoryType: MemoryType, position: Position): Option[Memory]
}

object Neocortex {

  def apply(): Neocortex = new NeocortexImpl()
}

private class NeocortexImpl extends Neocortex {

  val memories: Map[MemoryType, Memory] = Map()

  override def addMemory(memoryType: MemoryType, memory: Memory): Unit = {
    memories(memoryType) = memory
  }

  override def getMemeory(memoryType: MemoryType, position: Position): Option[Memory] = {
    memories.find(e => e._1==memoryType && e._2.locationalField.containsPosition(position)) match {
      case Some(tuple) => Some(tuple._2)
      case None => None
    }

  }
}

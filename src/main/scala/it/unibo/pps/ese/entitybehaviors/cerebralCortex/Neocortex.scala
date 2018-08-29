package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.view.Position


trait Neocortex {
  def addMemory(memoryType: MemoryType, memory: LongTermMemory)
  def getMemeory(memoryType: MemoryType, position: Position): Option[LongTermMemory]
}

object Neocortex {

  def apply(): Neocortex = new NeocortexImpl()
}

private class NeocortexImpl extends Neocortex {

  val memories: Map[MemoryType, LongTermMemory] = Map()

  override def addMemory(memoryType: MemoryType, memory: LongTermMemory): Unit = {
    memories(memoryType) = memory
  }

  override def getMemeory(memoryType: MemoryType, position: Position): Option[LongTermMemory] = {
    memories.find(e => e._1==memoryType && e._2.locationalField.containsPosition(position)) match {
      case Some(tuple) => Some(tuple._2)
      case None => None
    }

  }
}

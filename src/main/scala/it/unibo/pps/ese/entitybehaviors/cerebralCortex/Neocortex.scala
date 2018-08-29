package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.{LongTermMemory, ShortTermMemory}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.view.Position

import scala.collection.mutable.Map


trait Neocortex {
  def addMemory(memoryType: MemoryType, memory: LongTermMemory)
  def getMemeories(memoryType: MemoryType): Option[List[LongTermMemory]]
}

object Neocortex {

  def apply(): Neocortex = new NeocortexImpl()
}

private class NeocortexImpl extends Neocortex {

  type LongTermMemories = Map[MemoryType, List[LongTermMemory]]

  val memories: LongTermMemories = Map()

  override def addMemory(memoryType: MemoryType, memory: LongTermMemory): Unit = {
    memories.getOrElse(memoryType, List()).+:(memory)
  }

  override def getMemeories(memoryType: MemoryType): Option[List[LongTermMemory]] = {
    memories.get(memoryType)
  }
}

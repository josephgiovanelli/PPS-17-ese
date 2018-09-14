package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.{LongTermMemory, ShortTermMemory}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

@SerialVersionUID(100L)
private[cerebralCortex] trait Neocortex extends Serializable {
  def memories: Map[MemoryType, ListBuffer[LongTermMemory]]
  def addMemory(memoryType: MemoryType, memory: LongTermMemory)
  def getMemeories(memoryType: MemoryType): Option[ListBuffer[LongTermMemory]]
}

private[cerebralCortex] object Neocortex {

  def apply(): Neocortex = new NeocortexImpl()

  private class NeocortexImpl extends Neocortex {

    type LongTermMemories = Map[MemoryType, ListBuffer[LongTermMemory]]

    val memories: LongTermMemories = Map()

    override def addMemory(memoryType: MemoryType, memory: LongTermMemory): Unit = {
      memories.get(memoryType) match {
        case None => memories(memoryType) = ListBuffer()
        case _ =>
      }
      memories(memoryType) += memory
    }

    override def getMemeories(memoryType: MemoryType): Option[ListBuffer[LongTermMemory]] = {
      memories.get(memoryType)
    }
  }
}

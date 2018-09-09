package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.controller.saving.{Memento, Savable}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.{LongTermMemory, ShortTermMemory}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Neocortex.NeocortexMemento

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


private[cerebralCortex] trait Neocortex extends Savable[NeocortexMemento] {
  def memories: mutable.Map[MemoryType, ListBuffer[LongTermMemory]]
  def addMemory(memoryType: MemoryType, memory: LongTermMemory)
  def getMemeories(memoryType: MemoryType): Option[ListBuffer[LongTermMemory]]
}

object Neocortex {

  def apply(): Neocortex = new NeocortexImpl(None)
  def apply(neocortexMemento: NeocortexMemento): Neocortex = new NeocortexImpl(Some(neocortexMemento))

  type LongTermMemories = mutable.Map[MemoryType, ListBuffer[LongTermMemory]]

  private class NeocortexImpl(neocortexMemento: Option[NeocortexMemento]) extends Neocortex {

    val memories: LongTermMemories = neocortexMemento match {
      case Some(n) => n.memories
      case _ => mutable.Map()
    }


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

    override def serialize: NeocortexMemento = {
      NeocortexMemento(memories)
    }
  }

  case class NeocortexMemento(memories: LongTermMemories)

}

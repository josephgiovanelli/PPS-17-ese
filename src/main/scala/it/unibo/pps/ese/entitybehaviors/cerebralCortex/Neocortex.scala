package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.controller.saving.Memento
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.{LongTermMemory, ShortTermMemory}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.utils.Savable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


private[cerebralCortex] trait Neocortex extends Savable {
  def memories: mutable.Map[MemoryType, ListBuffer[LongTermMemory]]
  def addMemory(memoryType: MemoryType, memory: LongTermMemory)
  def getMemeories(memoryType: MemoryType): Option[ListBuffer[LongTermMemory]]
}

object Neocortex {

  def apply(): Neocortex = new NeocortexImpl()
  def apply(neocortexMemento: NeocortexMemento): Neocortex = new NeocortexImpl(neocortexMemento)

  type LongTermMemories = mutable.Map[MemoryType, ListBuffer[LongTermMemory]]

  private class NeocortexImpl(val memories: LongTermMemories = mutable.Map()) extends Neocortex {

    def this(neocortexMemento: NeocortexMemento) {
      this(neocortexMemento.memories)
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

    override def saveState(): Unit = {
      Memento.neocortexMemento = Some(NeocortexMemento(memories))
    }
  }

  case class NeocortexMemento(memories: LongTermMemories)

}

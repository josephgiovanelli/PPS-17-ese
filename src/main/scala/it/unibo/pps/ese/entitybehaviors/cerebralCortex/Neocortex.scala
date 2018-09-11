package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.controller.saving.{Memento, Savable, WorldMemento}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.{LongTermMemory, LongTermMemoryMemento, ShortTermMemory}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Neocortex.NeocortexMemento

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


private[cerebralCortex] trait Neocortex extends Savable[NeocortexMemento] {
  def memories: mutable.Map[MemoryType, ListBuffer[LongTermMemory]]
  def addMemory(memory: LongTermMemory)
  def getMemeories(memoryType: MemoryType): Option[ListBuffer[LongTermMemory]]
}

object Neocortex {

  def apply(): Neocortex = new NeocortexImpl(None)
  def apply(neocortexMemento: NeocortexMemento): Neocortex = new NeocortexImpl(Some(neocortexMemento))

  type LongTermMemories = mutable.Map[MemoryType, ListBuffer[LongTermMemory]]
  type LongTermMemoriesMemento = mutable.Map[Int, ListBuffer[LongTermMemoryMemento]]

  private class NeocortexImpl(neocortexMemento: Option[NeocortexMemento]) extends Neocortex {

    val memories: LongTermMemories = neocortexMemento match {
      case Some(n) => n.memories.map(t => (MemoryType(t._1), t._2.map(m => LongTermMemory(m))))
      case _ => mutable.Map()
    }


    override def addMemory(memory: LongTermMemory): Unit = {
      val memoryType = memory.memoryType
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
      NeocortexMemento(memories.map(t => (t._1.id, t._2.map(m => m.serialize match {
        case l: LongTermMemoryMemento => l
        case _ => throw new IllegalStateException("Neocortex can only hold LongTermMemory")}))))
    }
  }

  case class NeocortexMemento(memories: LongTermMemoriesMemento) extends Memento

}

package it.unibo.pps.ese.model.components.animals.brain.cerebralcortex

import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.Memory.{LongTermMemory, ShortTermMemory}
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.MemoryType

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

/**
  * The headquarters of the long term memories
  */
private[cerebralcortex] trait Neocortex {

  /**
    * Return all the saved memories
    *
    * @return a `Map` from `MemoryType` to `LongTermMemory`
    */
  def memories: Map[MemoryType, ListBuffer[LongTermMemory]]

  /**
    * Adds a memory
    *
    * @param memoryType the type of the `Memory`
    * @param memory the `LongTermMemory` to be added
    */
  def addMemory(memoryType: MemoryType, memory: LongTermMemory)

  /**
    * Return all the memories given a type
    *
    * @param memoryType the `MemoryType` of the memory
    * @return a `ListBuffer` of `LongTermMemory`
    */
  def getMemeories(memoryType: MemoryType): Option[ListBuffer[LongTermMemory]]
}

private[cerebralcortex] object Neocortex {

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

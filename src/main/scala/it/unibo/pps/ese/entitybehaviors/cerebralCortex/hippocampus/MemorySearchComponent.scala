package it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus

import it.unibo.pps.ese.controller.saving.{Memento, Savable}
import it.unibo.pps.ese.entitybehaviors.Direction
import it.unibo.pps.ese.entitybehaviors.Direction.Direction
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.{LocationalField, Memory, MemoryType, Position}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.{AbstractMemoryMemento, LongTermMemory, Memory, ShortTermMemory}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.MemorySearchComponent.MemorySearchComponentMemento

import scala.collection.mutable.ListBuffer


private[hippocampus] trait MemorySearchComponent extends Savable[MemorySearchComponentMemento] {
  def memoryType: MemoryType
  def updateTime()
  def hasNewMemory: Boolean
  def chooseNewMemory(currentPosition: Position): Option[Memory]
}

private[hippocampus] object MemorySearchComponent {

  def apply(memoryType: MemoryType, memories: ListBuffer[Memory]): MemorySearchComponent =
    new MemorySearchComponentImpl(
      memoryType,
      memories,
      None)

  def apply(memorySearchComponentMemento: MemorySearchComponentMemento): MemorySearchComponent =
    new MemorySearchComponentImpl(
      MemoryType(memorySearchComponentMemento.memoryType),
      memorySearchComponentMemento.memories.map(m => Memory(m)),
      memorySearchComponentMemento.currentMemory.map(m => Memory(m)))

  private class MemorySearchComponentImpl(val memoryType: MemoryType,
                                          val memories: ListBuffer[Memory],
                                          var currentMemory: Option[Memory]) extends MemorySearchComponent {

    override def updateTime(): Unit = {
      memories --= memories.filter(m => m match {
        case m: LongTermMemory => m.score<Hippocampus.longTermMemoryDeathThreashold
        case m: ShortTermMemory => m.elapsedTime>=Hippocampus.shortTermMemoryMaxTime
        case _ => true
      })
    }

    override def hasNewMemory: Boolean = currentMemory match {
      case None => memories.nonEmpty
      case _ => memories.size>1
    }

    override def chooseNewMemory(currentPosition: Position): Option[Memory] = {
      currentMemory match {
        case Some(memory) => memories -= memory
        case None =>
      }
      currentMemory = computeBestMemory(currentPosition)
      currentMemory
    }

    private def computeBestMemory(currentPosition: Position): Option[Memory] = {
      memories.size match {
        case 0 => None
        case _ => memories.find(m => m.locationalField.distanceFromPosition(currentPosition)==0) match {
          case Some(memory) => Some(memory)
          case None => Some(memories.maxBy(m => getMemoryCoefficient(m, currentPosition)))
        }
      }
    }

    private def getMemoryCoefficient(memory: Memory, position: Position): Double = {
      memory.score/memory.locationalField.distanceFromPosition(position)
    }

    override def serialize: MemorySearchComponentMemento = {
      MemorySearchComponentMemento(memoryType.id, memories.map(m => m.serialize), currentMemory.map(m => m.serialize))
    }
  }

  case class MemorySearchComponentMemento(memoryType: Int,
                                          memories: ListBuffer[AbstractMemoryMemento],
                                          currentMemory: Option[AbstractMemoryMemento]) extends Memento
}

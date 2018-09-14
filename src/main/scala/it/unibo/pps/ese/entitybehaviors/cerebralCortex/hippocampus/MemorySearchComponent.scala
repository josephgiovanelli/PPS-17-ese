package it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus

import it.unibo.pps.ese.entitybehaviors.Direction
import it.unibo.pps.ese.entitybehaviors.Direction.Direction
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.{LocationalField, Position}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.{LongTermMemory, Memory, ShortTermMemory}
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType

import scala.collection.mutable.ListBuffer

@SerialVersionUID(100L)
private[hippocampus] trait MemorySearchComponent extends Serializable {
  def memoryType: MemoryType
  def updateTime()
  def hasNewMemory: Boolean
  def chooseNewMemory(currentPosition: Position): Option[Memory]
}

private[hippocampus] object MemorySearchComponent {

  def apply(memoryType: MemoryType, memories: ListBuffer[Memory]): MemorySearchComponent =
    new MemorySearchComponentImpl(memoryType, memories)

  private class MemorySearchComponentImpl(val memoryType: MemoryType, val memories: ListBuffer[Memory]) extends MemorySearchComponent {
    var currentMemory: Option[Memory] = None

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
  }
}

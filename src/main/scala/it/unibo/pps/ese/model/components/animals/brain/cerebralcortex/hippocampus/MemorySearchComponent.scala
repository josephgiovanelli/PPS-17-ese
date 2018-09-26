package it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus

import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.Memory.{LongTermMemory, Memory, ShortTermMemory}
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.MemoryType
import it.unibo.pps.ese.utils.Position

import scala.collection.mutable.ListBuffer

/**
  * The utility component for a search.
  * It has to be created every time a new search starts.
  *
  * Every memory is considered only one time per serach.
  */
private[hippocampus] trait MemorySearchComponent {

  /**
    *
    * @return the `MemoryType` of the search
    */
  def memoryType: MemoryType

  /**
    * Updates the time of the memories
    */
  def updateTime()

  /**
    *
    * @return if there are others memories not already used
    */
  def hasNewMemory: Boolean

  /**
    * Choose the new best memory for the search.
    * Before calling this methods the method `hasNewMemory` has to be called to verify
    * that there are others memories.
    *
    * @param currentPosition the current `Position`
    * @return the new best `Memory`
    */
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

    /* The best memory is choosen by computing a memory coefficient which express his convenience */
    private def computeBestMemory(currentPosition: Position): Option[Memory] = {
      memories.size match {
        case 0 => None
        case _ => memories.find(m => m.locationalField.distanceFromPosition(currentPosition)==0) match {
          case Some(memory) => Some(memory)
          case None => Some(memories.maxBy(m => getMemoryCoefficient(m, currentPosition)))
        }
      }
    }

    /* The coefficient is obtained dividing the score of a memory by the distance from the actual position.
    *
    * The score, for a memory, indicates the probability to complete successfully the search in that place.
    * The distance indicates the time necessary to reach that position.
    */
    private def getMemoryCoefficient(memory: Memory, position: Position): Double = {
      memory.score/memory.locationalField.distanceFromPosition(position)
    }
  }
}

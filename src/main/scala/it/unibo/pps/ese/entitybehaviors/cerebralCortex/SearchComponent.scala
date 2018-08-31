package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.Direction
import it.unibo.pps.ese.entitybehaviors.Direction.Direction
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.ShortTermMemory
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.view.Position

import scala.collection.mutable.ListBuffer

case class SimpleMemory(locationalField: LocationalField, memoryCoefficient: Double) {
  def computeDirection(currentPosition: Position): Option[Direction] = {
    locationalField.distanceFromPosition(currentPosition) match {
      case 0 => None
      case _ => Some(findDirection(currentPosition))
    }
  }

  private def findDirection(currentPosition: Position): Direction = {
    val centerPosition = locationalField.centerPosition
    val xDistance = centerPosition.x - currentPosition.x
    val yDistance = centerPosition.y - currentPosition.y
    var xDirection = Direction.RIGHT
    var yDirection = Direction.UP
    if (xDistance<0) xDirection = Direction.LEFT
    if (yDistance<0) yDirection = Direction.DOWN

    if(Math.abs(xDistance)>Math.abs(yDistance)) xDirection else yDirection
  }
}


trait SearchComponent {
  def chooseNewMemory(currentPosition: Position): Option[SimpleMemory]
}

object SearchComponent {

  def apply(memories: ListBuffer[SimpleMemory]): SearchComponent = new SearchComponentImpl(memories)

  private class SearchComponentImpl(memories: ListBuffer[SimpleMemory]) extends SearchComponent {
    var currentMemory: Option[SimpleMemory] = None

    override def chooseNewMemory(currentPosition: Position): Option[SimpleMemory] = {
      currentMemory match {
        case Some(memory) => memories -= memory
        case None =>
      }
      currentMemory = computeBestMemory(currentPosition)
      currentMemory
    }

    private def computeBestMemory(currentPosition: Position): Option[SimpleMemory] = {
      memories.size match {
        case 0 => None
        case _ => memories.find(m => m.locationalField.distanceFromPosition(currentPosition)==0) match {
          case Some(memory) => Some(memory)
          case None => Some(memories.maxBy(m => m.memoryCoefficient))
        }
      }
    }

  }
}

package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.view.Position


object Location {
  case class LocationalField(startPosition: Position, endPosition: Position) {
    def containsPosition(position: Position): Boolean = {
      position >= startPosition && position <= endPosition
    }
  }
}

trait Hippocampus {
  def updateTime()
  def notifyEvent(memoryType: MemoryType, position: Position)

  def computeDirection(memoryType: MemoryType, currentPosition: Position)
}

object Hippocampus {
  def apply(neocortex: Neocortex): Hippocampus = new HippocampusImpl(neocortex)
}

class HippocampusImpl(neocortex: Neocortex) extends Hippocampus {

  val memories: Map[MemoryType, ShortTermMemory] = Map()

  override def notifyEvent(memoryType: MemoryType, position: Position): Unit = {
    neocortex.getMemeory(memoryType, position)

  }

  override def computeDirection(memoryType: MemoryType, currentPosition: Position): Unit = ???

  override def updateTime(): Unit = ???
}

package it.unibo.pps.ese.entitybehaviors.cerebralCortex


import it.unibo.pps.ese.entitybehaviors.Direction.Direction
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory._
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.view.Position

import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Hippocampus {
  def updateTime()
  def notifyEvent(memoryType: MemoryType, position: Position)
  def startSearch(memoryType: MemoryType, position: Position)
}

object Hippocampus {

  val locationalFieldSize = 5
  var worldWidth: Int = 1000
  var worldHeight: Int = 1000

  def apply(worldWidth: Int, worldHeight: Int, neocortex: Neocortex): Hippocampus = {
    this.worldWidth=worldWidth
    this.worldHeight=worldHeight
    new HippocampusImpl(worldWidth, worldHeight, neocortex)
  }

  private class HippocampusImpl(worldWidth: Int, worldHeight: Int, neocortex: Neocortex) extends Hippocampus {

    import scala.collection.mutable.Map

    type ShortTermMemoryMap = Map[MemoryType, ListBuffer[ShortTermMemory]]
    type LongTermMemoryMap = Map[MemoryType, ListBuffer[LongTermMemory]]

    val shortTermMemoryMaxTime = 5

    val eventGainMin = 40
    val eventGainMax = 60
    val eventGain = new Random()
    val longTermThreshold = 100

    val memories: ShortTermMemoryMap = Map()
    var searchMemories: (ShortTermMemoryMap, LongTermMemoryMap) =
      (Map[MemoryType, ListBuffer[ShortTermMemory]](), Map[MemoryType, ListBuffer[LongTermMemory]]())


    override def notifyEvent(memoryType: MemoryType, position: Position): Unit = {
      neocortex.getMemeories(memoryType) match {
        case Some(list) =>
          list.find(m => m.locationalField == LocationalField(position)) match {
            case Some(memory) => memory.score += computeGain
            case None => addNewShortTermMemory(memoryType, position)
        }
        case None => addNewShortTermMemory(memoryType, position)
      }
    }

    override def updateTime(): Unit = {
      memories.foreach(e => e._2.foreach(m => m.incrementElapsedTime()))
      memories.foreach(t => t._2 --= t._2.filter(m => m.elapsedTime>=shortTermMemoryMaxTime))
    }

    private def computeGain: Double = {
      eventGainMin + (eventGainMax-eventGainMin)*eventGain.nextDouble()
    }

    private def getMemoryCoefficient(memory: ShortTermMemory, position: Position): Double = {
      memory.score/memory.locationalField.distanceFromPosition(position)
    }

    private def addNewShortTermMemory(memoryType: MemoryType, position: Position) = {
      memories.get(memoryType) match {
        case None => memories(memoryType) = ListBuffer()
        case _ =>
      }
      memories(memoryType) += ShortTermMemory(memoryType, position, computeGain)
    }

  }
}

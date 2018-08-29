package it.unibo.pps.ese.entitybehaviors.cerebralCortex


import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory._
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.view.Position

import scala.collection.mutable.Map
import scala.util.Random

trait Hippocampus {
  def updateTime()
  def notifyEvent(memoryType: MemoryType, position: Position)
  def computeDirection(memoryType: MemoryType, currentPosition: Position)
}

object Hippocampus {

  val locationalFieldSize = 5

  def apply(neocortex: Neocortex): Hippocampus = new HippocampusImpl(neocortex)

  private class HippocampusImpl(neocortex: Neocortex) extends Hippocampus {

    type ShortTermMeories = Map[MemoryType, ShortTermMemory]

    val shortTermMemoryMaxTime = 5

    val eventGainMin = 40
    val eventGainMax = 60
    val eventGain = new Random()
    val longTermThreshold = 100

    val memories: ShortTermMeories = Map()
    var searchMemories: ShortTermMeories = Map()


    override def notifyEvent(memoryType: MemoryType, position: Position): Unit = {
      searchMemories = memories.clone()
      neocortex.getMemeories(memoryType) match {
        case Some(list) => {

        }
        case None => {
          memories(memoryType) = ShortTermMemory(position,computeGain)
        }
      }
    }

    override def computeDirection(memoryType: MemoryType, currentPosition: Position): Unit = {
      neocortex.getMemeories(memoryType) match {
        case Some(list) => {

        }
        case None =>
      }
      getBestMemory(currentPosition)
    }

    override def updateTime(): Unit = {
      for (memory <- memories.values) {
        memory.elapsedTime+=1
      }
      memories.retain((k,v) => v.elapsedTime>=shortTermMemoryMaxTime)
    }

    private def computeGain: Double = {
      eventGainMin + (eventGainMax-eventGainMin)*eventGain.nextDouble()
    }

    private def getBestMemory(position: Position): ShortTermMemory = {
      searchMemories.maxBy(t => getMemoryCoefficient(t._2, position))._2
    }

    private def getMemoryCoefficient(memory: ShortTermMemory, position: Position): Double = {
      memory.score/memory.locationalField.distanceFromPosition(position)
    }

  }
}

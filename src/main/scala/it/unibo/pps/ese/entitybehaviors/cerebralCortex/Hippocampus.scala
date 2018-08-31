package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory._
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.view.Position

import scala.collection.mutable.{ListBuffer, Map}
import scala.util.Random

trait Hippocampus {
  def updateTime()
  def notifyEvent(memoryType: MemoryType, position: Position)
  def computeDirection(memoryType: MemoryType, currentPosition: Position)
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

    type ShortTermMeories = Map[MemoryType, ListBuffer[ShortTermMemory]]

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
          memories.get(memoryType) match {
            case None => memories(memoryType) = ListBuffer()
            case _ =>
          }
          memories(memoryType) += ShortTermMemory(position,computeGain)
        }
      }
    }

    override def computeDirection(memoryType: MemoryType, currentPosition: Position): Unit = {
      neocortex.getMemeories(memoryType) match {
        case Some(list) => {

        }
        case None =>
      }
      computeBestMemory(memoryType, currentPosition)
    }

    override def updateTime(): Unit = {
      memories.foreach(e => e._2.foreach(m => m.incrementElapsedTime()))
      memories.foreach(t => t._2 --= t._2.filter(m => m.elapsedTime>=shortTermMemoryMaxTime))
    }

    private def computeGain: Double = {
      eventGainMin + (eventGainMax-eventGainMin)*eventGain.nextDouble()
    }

    private def computeBestMemory(memoryType: MemoryType, position: Position): ShortTermMemory = {
      val list = searchMemories.getOrElse(memoryType, List())
      list.find(m => m.locationalField.distanceFromPosition(position)==0) match {
        case Some(memory) => memory
        case None => list.maxBy(m => getMemoryCoefficient(m, position))
      }
    }

    private def getMemoryCoefficient(memory: ShortTermMemory, position: Position): Double = {
      memory.score/memory.locationalField.distanceFromPosition(position)
    }

  }
}

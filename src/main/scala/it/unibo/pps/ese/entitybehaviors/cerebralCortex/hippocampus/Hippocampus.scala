package it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus

import it.unibo.pps.ese.entitybehaviors.Direction
import it.unibo.pps.ese.entitybehaviors.Direction.Direction
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory._
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.{Memory => _, _}
import it.unibo.pps.ese.view.Position

import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Hippocampus {
  def updateTime()
  def notifyEvent(memoryType: MemoryType, position: Position)
  def searchStarted: Boolean
  def startNewSearch(memoryType: MemoryType)
  def hasNewMemory: Boolean
  def chooseNewMemory(currentPosition: Position)
  def computeDirection(currentPosition: Position): Direction
}

object Hippocampus {

  val locationalFieldSize = 5
  var worldWidth: Int = 1000
  var worldHeight: Int = 1000

  val shortTermMemoryMaxTime = 5
  val longTermMemoryDeathThreashold = 1


  def apply(worldWidth: Int, worldHeight: Int, neocortex: Neocortex): Hippocampus = {
    this.worldWidth=worldWidth
    this.worldHeight=worldHeight
    new HippocampusImpl(worldWidth, worldHeight, neocortex)
  }

  private class HippocampusImpl(worldWidth: Int, worldHeight: Int, neocortex: Neocortex) extends Hippocampus {

    import scala.collection.mutable.Map

    type ShortTermMemoryMap = Map[MemoryType, ListBuffer[ShortTermMemory]]

    val longTermMemoryThreshold = 99
    val eventGainMin = 40
    val eventGainMax = 60
    val eventGain = new Random()

    val memories: ShortTermMemoryMap = Map()

    var memorySearchComponent: Option[MemorySearchComponent] = None
    var currentBestMemory: Option[Memory] = None

    override def notifyEvent(memoryType: MemoryType, position: Position): Unit = {
      neocortex.getMemeories(memoryType) match {
        case Some(list) =>
          list.find(m => m.locationalField == LocationalField(position)) match {
            case Some(memory) =>
              memory.score += computeGain
            case None => checkShortTermMemory(memoryType, position)
        }
        case None => checkShortTermMemory(memoryType, position)
      }
      if(memorySearchComponent.isDefined && memoryType==memorySearchComponent.get.memoryType) {
        memorySearchComponent = None
      }
    }

    override def updateTime(): Unit = {
      memories.foreach(t => t._2 --= t._2.filter(m => {m.updateTime(); m.elapsedTime}>=shortTermMemoryMaxTime))
      neocortex.memories.foreach(t => t._2 --= t._2.filter(m => {m.updateTime(); m.score}<longTermMemoryDeathThreashold))
    }

    override def searchStarted: Boolean = memorySearchComponent.isDefined

    override def startNewSearch(memoryType: MemoryType): Unit = {
      memorySearchComponent = Some(MemorySearchComponent(
        memoryType,
        ListBuffer() ++= memories.getOrElse(memoryType, List()) ++
          neocortex.getMemeories(memoryType).getOrElse(List())
      ))
    }

    override def hasNewMemory: Boolean = {
      memorySearchComponent match {
        case None => throw new IllegalStateException("Search not started")
        case Some(value) => value.hasNewMemory
      }
    }

    override def chooseNewMemory(currentPosition: Position): Unit = {
      memorySearchComponent match {
        case None => throw new IllegalStateException("Search not started")
        case Some(value) => currentBestMemory = value.chooseNewMemory(currentPosition)
      }
    }

    override def computeDirection(currentPosition: Position): Direction = {
      currentBestMemory match {
        case None => throw new IllegalStateException("Memory not defined")
        case Some(memory) => findDirection(currentPosition, memory.locationalField.centerPosition)
      }
    }

    private def computeGain: Double = {
      eventGainMin + (eventGainMax-eventGainMin)*eventGain.nextDouble()
    }

    private def checkShortTermMemory(memoryType: MemoryType, position: Position) = {
      memories.get(memoryType) match {
        case None =>
          memories(memoryType) = ListBuffer()
          addShortTermMemory(memoryType, position)
        case Some(list) => list.find(m => m.locationalField.distanceFromPosition(position)==0) match {
          case None => addShortTermMemory(memoryType, position)
          case Some(memory) =>
            memory.score += computeGain
            if (memory.score > longTermMemoryThreshold) {
              neocortex.addMemory(memoryType, LongTermMemory(memory.memoryType, memory.locationalField, memory.score))
            }
        }
      }
    }

    private def addShortTermMemory(memoryType: MemoryType, position: Position) = {
      memories(memoryType) += ShortTermMemory(memoryType, position, computeGain)
    }

    private def findDirection(fromPosition: Position, toPosition: Position): Direction = {
      val xDistance = toPosition.x - fromPosition.x
      val yDistance = toPosition.y - fromPosition.y
      var xDirection = Direction.RIGHT
      var yDirection = Direction.DOWN
      if (xDistance<0) xDirection = Direction.LEFT
      if (yDistance<0) yDirection = Direction.UP

      if(Math.abs(xDistance)>Math.abs(yDistance)) xDirection else yDirection
    }
  }
}

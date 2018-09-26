package it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus

import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.Memory._
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.{LocationalField, MemoryType, Neocortex}
import it.unibo.pps.ese.model.components.animals.brain.Direction
import it.unibo.pps.ese.model.components.animals.brain.Direction.Direction
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus.Hippocampus.SearchingState
import it.unibo.pps.ese.utils.Position

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * The headquarters of the short term memories
  * and of the management of the memory in general.
  *
  * It takes care of the creation of new short term memories, the updating of their status
  * and it moves the most frequent memories to the neocortex, creating long term memories.
  *
  * It is also the responsible of the management of memories during a search,
  * choosing the best memories.
  */
trait Hippocampus {

  /**
    *
    * @return the state of the search
    */
  def searchingState: SearchingState

  /**
    * Updates the memories' time
    */
  def updateTime()

  /**
    * Notifies an event, the `Hippocampus` creates a new memory or strengthens an existing one,
    * based on the the `LoactionalField` of the memory
    *
    * @param memoryType the type of the memory
    * @param position the position of the event
    */
  def notifyEvent(memoryType: MemoryType, position: Position)

  /**
    * Starts a new search based on the type of the memory
    *
    * @param memoryType the type of the memory
    */
  def startNewSearch(memoryType: MemoryType)

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
  def chooseNewMemory(currentPosition: Position)

  /**
    * Computes the `Direction` to approach to the position of the memory
    *
    * @param currentPosition the current `Position`
    * @return the `Direction`
    */
  def computeDirection(currentPosition: Position): Direction
}

object Hippocampus {

  /**
    * The state of the search
    */
  trait SearchingState

  /**
    * No search started
    */
  case object Inactive extends SearchingState

  /**
    * An active started search.
    * There are some others memories available
    */
  case object Active extends SearchingState

  /**
    * An ended started search.
    * No more memories available
    */
  case object Ended extends SearchingState

  val longTermMemoryThreshold = 99
  val eventGainMin = 40
  val eventGainMax = 60
  val shortTermMemoryMaxTime = 30
  val longTermMemoryDeathThreashold = 1

  def apply(worldWidth: Int, worldHeight: Int, locationalFieldSize: Double): Hippocampus =
    new HippocampusImpl(worldWidth, worldHeight, locationalFieldSize)


  private class HippocampusImpl(val worldWidth: Int, val worldHeight: Int, val locationalFieldSize: Double) extends Hippocampus {

    import scala.collection.mutable.Map

    type ShortTermMemoryMap = Map[MemoryType, ListBuffer[ShortTermMemory]]


    val neocortex: Neocortex = Neocortex()
    val memories: ShortTermMemoryMap = Map()
    val eventGain = new Random()
    var memorySearchComponent: Option[MemorySearchComponent] = None
    var currentBestMemory: Option[Memory] = None

    var searchingState: SearchingState = Inactive

    override def notifyEvent(memoryType: MemoryType, position: Position): Unit = {
      neocortex.getMemeories(memoryType) match {
        case Some(list) =>
          list.find(m => m.locationalField == LocationalField(worldWidth, worldHeight, locationalFieldSize, position)) match {
            case Some(memory) =>
              memory.score += computeGain
            case None => checkShortTermMemory(memoryType, position)
        }
        case None => checkShortTermMemory(memoryType, position)
      }
      if(memorySearchComponent.isDefined && memoryType==memorySearchComponent.get.memoryType) {
        memorySearchComponent = None
        searchingState = Inactive
      }
    }

    override def updateTime(): Unit = {
      memories.foreach(t => t._2 --= t._2.filter(m => {m.updateTime(); m.elapsedTime}>=shortTermMemoryMaxTime))
      neocortex.memories.foreach(t => t._2 --= t._2.filter(m => {m.updateTime(); m.score}<longTermMemoryDeathThreashold))
      if (memorySearchComponent.isDefined) memorySearchComponent.get.updateTime()
    }

    override def startNewSearch(memoryType: MemoryType): Unit = {

      val list = ListBuffer() ++= memories.getOrElse(memoryType, List()) ++ neocortex.getMemeories(memoryType).getOrElse(List())
      if (list.nonEmpty)searchingState = Active else searchingState = Ended
      memorySearchComponent = Some(MemorySearchComponent(memoryType, list))
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
        case Some(memory) =>
          val direction = findDirection(currentPosition, memory.locationalField.centerPosition)
          if (direction==Direction.NONE && !hasNewMemory) searchingState = Ended
          direction
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
      memories(memoryType) += ShortTermMemory(memoryType, LocationalField(worldWidth, worldHeight, locationalFieldSize, position), computeGain)
    }

    private def findDirection(fromPosition: Position, toPosition: Position): Direction = {
      val distance = LocationalField(worldWidth, worldHeight, locationalFieldSize, toPosition).distanceFromPosition(fromPosition)
      if (distance==0) Direction.NONE else {
        val xDistance = toPosition.x - fromPosition.x
        val yDistance = toPosition.y - fromPosition.y
        var xDirection = Direction.RIGHT
        var yDirection = Direction.DOWN
        if (xDistance<0) xDirection = Direction.LEFT
        if (yDistance<0) yDirection = Direction.UP

        if(Math.abs(xDistance)>Math.abs(yDistance)) xDirection else yDirection

        (Math.abs(xDistance), Math.abs(yDistance)) match {
          case (x,y) if x>=y => xDirection
          case _ => yDirection
        }
      }

    }
  }
}

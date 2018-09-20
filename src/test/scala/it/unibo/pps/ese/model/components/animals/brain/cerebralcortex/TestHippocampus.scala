package it.unibo.pps.ese.model.components.animals.brain.cerebralcortex

import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus.Hippocampus
import it.unibo.pps.ese.model.components.animals.brain.Direction
import it.unibo.pps.ese.utils.Position
import org.scalatest.FunSuite

class TestHippocampus extends FunSuite {

  private object Properties {
    val worldWidth = 1000
    val worldHeight = 1000
    val locationalFieldSize = 5
  }

  test("Test add and use short term memories") {
    import Properties._
    val h: Hippocampus = Hippocampus(worldWidth, worldHeight, locationalFieldSize)
    h.notifyEvent(MemoryType.HUNTING, Position(25,20))
    assertThrows[IllegalStateException](h.computeDirection(Position(0,0)))
    assertThrows[IllegalStateException](h.chooseNewMemory(Position(0,0)))
    h.startNewSearch(MemoryType.HUNTING)
    assert(h.hasNewMemory)
    h.chooseNewMemory(Position(0,0))
    assert(h.computeDirection(Position(0,0))==Direction.RIGHT)
    assert(h.computeDirection(Position(15,0))==Direction.DOWN)
    assert(h.computeDirection(Position(25,20))==Direction.NONE)

    h.notifyEvent(MemoryType.HUNTING, Position(50,50))
    h.startNewSearch(MemoryType.HUNTING)
    assert(h.hasNewMemory)
    h.chooseNewMemory(Position(80,70))
    assert(h.computeDirection(Position(80,70))==Direction.LEFT)
    assert(h.computeDirection(Position(40,30))==Direction.DOWN)
    assert(h.hasNewMemory)
    h.chooseNewMemory(Position(40,30))
    assert(h.computeDirection(Position(40,30))==Direction.LEFT)
    assert(!h.hasNewMemory)
  }

  test("Test short term memory") {
    import Properties._
    val h: Hippocampus = Hippocampus(worldWidth, worldHeight, locationalFieldSize)
    h.notifyEvent(MemoryType.HUNTING, Position(25,20))
    h.notifyEvent(MemoryType.HUNTING, Position(50,60))
    for (i <- 0 until Hippocampus.shortTermMemoryMaxTime) {
      h.updateTime()
    }
    h.startNewSearch(MemoryType.HUNTING)
    assert(!h.hasNewMemory)
  }

  test("Test life of short term memory while searching") {
    import Properties._
    val h: Hippocampus = Hippocampus(worldWidth, worldHeight, locationalFieldSize)
    h.notifyEvent(MemoryType.HUNTING, Position(25,20))
    h.notifyEvent(MemoryType.HUNTING, Position(50,60))
    h.startNewSearch(MemoryType.HUNTING)
    for (i <- 0 until Hippocampus.shortTermMemoryMaxTime) {
      h.updateTime()
    }
    assert(!h.hasNewMemory)
  }

  test("Test long term short memories") {
    import Properties._
    val h: Hippocampus = Hippocampus(worldWidth, worldHeight, locationalFieldSize)
    val repetitionValue = 5

    for (i <- 0 until repetitionValue) {
      h.notifyEvent(MemoryType.HUNTING, Position(0,0))
    }
    for (i <- 0 until Hippocampus.shortTermMemoryMaxTime) {
      h.updateTime()
    }
    h.startNewSearch(MemoryType.HUNTING)
    assert(h.hasNewMemory)
    for (i <- 0 until repetitionValue*Hippocampus.eventGainMax) {
      h.updateTime()
    }
    assert(!h.hasNewMemory)

    h.notifyEvent(MemoryType.HUNTING, Position(0,0))
    h.notifyEvent(MemoryType.HUNTING, Position(10,0))
    h.notifyEvent(MemoryType.HUNTING, Position(5,8))
    h.startNewSearch(MemoryType.HUNTING)
    for (i <- 0 until Hippocampus.shortTermMemoryMaxTime) {
      h.updateTime()
    }
    assert(h.hasNewMemory)
    for (i <- 0 until repetitionValue*Hippocampus.eventGainMax) {
      h.updateTime()
    }
    assert(!h.hasNewMemory)
  }
}

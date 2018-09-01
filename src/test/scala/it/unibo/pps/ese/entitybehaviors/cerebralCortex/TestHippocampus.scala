package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.Direction
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.Hippocampus
import it.unibo.pps.ese.view.Position
import org.scalatest.FunSuite

class TestHippocampus extends FunSuite {

  private object Properties {
    val worldWidth = 1000
    val worldHeight = 1000
  }

  test("Test add and use short term memories") {
    import Properties._
    val h: Hippocampus = Hippocampus(worldWidth, worldHeight, Neocortex())
    h.notifyEvent(MemoryType.Hunting, Position(25,20))
    assertThrows[IllegalStateException](h.computeDirection(Position(0,0)))
    assertThrows[IllegalStateException](h.chooseNewMemory(Position(0,0)))
    h.startNewSearch(MemoryType.Hunting)
    assert(h.hasNewMemory)
    h.chooseNewMemory(Position(0,0))
    assert(h.computeDirection(Position(0,0))==Direction.RIGHT)
    assert(h.computeDirection(Position(15,0))==Direction.DOWN)
    assert(h.computeDirection(Position(25,20))==Direction.NONE)

    h.notifyEvent(MemoryType.Hunting, Position(50,50))
    h.startNewSearch(MemoryType.Hunting)
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
    val h: Hippocampus = Hippocampus(worldWidth, worldHeight, Neocortex())
    h.notifyEvent(MemoryType.Hunting, Position(25,20))
    h.notifyEvent(MemoryType.Hunting, Position(50,60))
    for (i <- 0 until Hippocampus.shortTermMemoryMaxTime) {
      h.updateTime()
    }
    h.startNewSearch(MemoryType.Hunting)
    assert(!h.hasNewMemory)
  }

  test("Test life of short term memory while searching") {
    import Properties._
    val h: Hippocampus = Hippocampus(worldWidth, worldHeight, Neocortex())
    h.notifyEvent(MemoryType.Hunting, Position(25,20))
    h.notifyEvent(MemoryType.Hunting, Position(50,60))
    h.startNewSearch(MemoryType.Hunting)
    for (i <- 0 until Hippocampus.shortTermMemoryMaxTime) {
      h.updateTime()
    }
    assert(!h.hasNewMemory)
  }

  test("Test long term short memories") {
    import Properties._
    val h: Hippocampus = Hippocampus(worldWidth, worldHeight, Neocortex())
    val repetitionValue = 5

    for (i <- 0 until repetitionValue) {
      h.notifyEvent(MemoryType.Hunting, Position(0,0))
    }
    for (i <- 0 until Hippocampus.shortTermMemoryMaxTime) {
      h.updateTime()
    }
    h.startNewSearch(MemoryType.Hunting)
    assert(h.hasNewMemory)
    for (i <- 0 until repetitionValue*Hippocampus.eventGainMax) {
      h.updateTime()
    }
    assert(!h.hasNewMemory)

    h.notifyEvent(MemoryType.Hunting, Position(0,0))
    h.notifyEvent(MemoryType.Hunting, Position(10,0))
    h.notifyEvent(MemoryType.Hunting, Position(5,8))
    h.startNewSearch(MemoryType.Hunting)
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

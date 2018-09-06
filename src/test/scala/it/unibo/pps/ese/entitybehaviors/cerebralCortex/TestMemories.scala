package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import org.scalatest.FunSuite
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory._
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.Hippocampus

class TestMemories extends FunSuite {

  private object Properties {
    val worldWidth = 1000
    val worldHeight = 1000
    val locationalFieldSize = 5
  }

  test("Test locational fields") {
    import Properties._

    var locationalField: LocationalField = LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(10,10))
    assert(locationalField.topLeftPosition==Position(10-locationalFieldSize, 10-locationalFieldSize))
    assert(locationalField.bottomRightPosition==Position(10+locationalFieldSize, 10+locationalFieldSize))

    locationalField = LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(worldWidth+10, worldHeight-20))
    assert(locationalField.centerPosition==Position(worldWidth-locationalFieldSize, worldHeight-20))

    locationalField = LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(0,0))
    assert(locationalField.centerPosition==Position(locationalFieldSize,locationalFieldSize))
    assert(locationalField.distanceFromPosition(Position(0,0))==0)

    var p = Position(locationalFieldSize+10,locationalFieldSize)
    assert(locationalField.distanceFromPosition(p)==10)

    p = Position(locationalFieldSize+6, locationalFieldSize+8)
    assert(locationalField.distanceFromPosition(p)==10)
  }

  test("Test long term memories") {
    import Properties._

    var locationalField: LocationalField = LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(0, 20))
    var score = 50
    var longTermMemory: LongTermMemory = LongTermMemory(MemoryType.HUNTING, locationalField, 50)
    longTermMemory.score+=60
    assert(longTermMemory.locationalField==LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(locationalFieldSize,20)))
    assert(longTermMemory.score==110)
  }

  test("Test short term memories") {
    import Properties._

    val locationalField: LocationalField = LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(0, 20))
    val score = 50
    val shortTermMemory: ShortTermMemory = ShortTermMemory(MemoryType.HUNTING, locationalField, score)
    shortTermMemory.updateTime()
    shortTermMemory.updateTime()
    assert(shortTermMemory.locationalField==LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(locationalFieldSize,20)))
    assert(shortTermMemory.elapsedTime==2)
  }
}

package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import org.scalatest.FunSuite
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory._
import it.unibo.pps.ese.view.Position
import hippocampus.Hippocampus._

class TestMemories extends FunSuite {
  test("Test locational fields") {
    var locationalField: LocationalField = LocationalField(Position(10,10))
    assert(locationalField.topLeftPosition==Position(10-locationalFieldSize, 10-locationalFieldSize))
    assert(locationalField.bottomRightPosition==Position(10+locationalFieldSize, 10+locationalFieldSize))

    locationalField = LocationalField(Position(worldWidth+10, worldHeight-20))
    assert(locationalField.centerPosition==Position(worldWidth-locationalFieldSize, worldHeight-20))

    locationalField = LocationalField(Position(0,0))
    assert(locationalField.centerPosition==Position(locationalFieldSize,locationalFieldSize))
    assert(locationalField.distanceFromPosition(Position(0,0))==0)

    var p = Position(locationalFieldSize+10,locationalFieldSize)
    assert(locationalField.distanceFromPosition(p)==10)

    p = Position(locationalFieldSize+6, locationalFieldSize+8)
    assert(locationalField.distanceFromPosition(p)==10)
  }

  test("Test long term memories") {
    var locationalField: LocationalField = LocationalField(Position(0, 20))
    var score = 50
    var longTermMemory: LongTermMemory = LongTermMemory(MemoryType.Hunting, locationalField, 50)
    longTermMemory.score+=60
    assert(longTermMemory.locationalField==LocationalField(Position(locationalFieldSize,20)))
    assert(longTermMemory.score==110)
  }

  test("Test short term memories") {
    val locationalField: LocationalField = LocationalField(Position(0, 20))
    val score = 50
    val shortTermMemory: ShortTermMemory = ShortTermMemory(MemoryType.Hunting, locationalField, score)
    shortTermMemory.updateTime()
    shortTermMemory.updateTime()
    assert(shortTermMemory.locationalField==LocationalField(Position(locationalFieldSize,20)))
    assert(shortTermMemory.elapsedTime==2)
  }
}

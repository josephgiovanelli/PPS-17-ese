package it.unibo.pps.ese.entitybehaviors.cerebralcortex

import it.unibo.pps.ese.entitybehaviors.cerebralcortex.Memory.{LongTermMemory, ShortTermMemory}
import org.scalatest.FunSuite

class TestNeocortex extends FunSuite {

  private object Properties {
    val worldWidth = 1000
    val worldHeight = 1000
    val locationalFieldSize = 5
  }

  test("Test neocortex") {
    import Properties._

    val neocortex: Neocortex = Neocortex()
    neocortex.addMemory(MemoryType.HUNTING, LongTermMemory(MemoryType.HUNTING, LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(0,20)), 150))
    neocortex.addMemory(MemoryType.HUNTING, LongTermMemory(MemoryType.HUNTING, LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(10,30)), 160))
    neocortex.addMemory(MemoryType.COUPLE, LongTermMemory(MemoryType.HUNTING, LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(100,130)), 260))
    assert(neocortex.getMemeories(MemoryType.HUNTING).get.size==2)
    assert(neocortex.getMemeories(MemoryType.COUPLE).get.size==1)
  }
}

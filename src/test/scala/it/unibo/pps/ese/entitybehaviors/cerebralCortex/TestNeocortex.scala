package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.{LongTermMemory, ShortTermMemory}
import it.unibo.pps.ese.view.Position
import org.scalatest.FunSuite

class TestNeocortex extends FunSuite {
  test("Test neocortex") {
    val neocortex: Neocortex = Neocortex()
    neocortex.addMemory(MemoryType.Hunting, LongTermMemory(MemoryType.Hunting, LocationalField(Position(0,20)), 150))
    neocortex.addMemory(MemoryType.Hunting, LongTermMemory(MemoryType.Hunting, LocationalField(Position(10,30)), 160))
    neocortex.addMemory(MemoryType.Reproduction, LongTermMemory(MemoryType.Hunting, LocationalField(Position(100,130)), 260))
    assert(neocortex.getMemeories(MemoryType.Hunting).get.size==2)
    assert(neocortex.getMemeories(MemoryType.Reproduction).get.size==1)
  }
}

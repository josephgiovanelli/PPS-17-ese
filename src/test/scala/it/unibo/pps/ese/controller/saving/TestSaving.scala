package it.unibo.pps.ese.controller.saving

import java.nio.ByteBuffer

import org.scalatest.FunSuite
import boopickle.Default._
import it.unibo.pps.ese.entitybehaviors.ReproductionComponent
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory._
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Neocortex.NeocortexMemento
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.Hippocampus
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.hippocampus.Hippocampus.HippocampusMemento
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.{LocationalField, MemoryType, Neocortex, Position}
import it.unibo.pps.ese.genericworld.model.{BrainComponentMemento, Entity}
import it.unibo.pps.ese.genericworld.model.Entity.{AbstractEntityMemento, ImprovedEntityMemento}

import scala.concurrent.ExecutionContext


class TestSaving extends FunSuite {

/*  trait SavingTool[T, M] {
    def save(toSave: T): SavingTool[T, M]
    def load: T

  }

  object SavingTool {
    def apply[T, M](): SavingTool[T, M] = new SavingToolImpl[T, M]()

    class SavingToolImpl[T, M] extends SavingTool[T, M] {

      private val buf: Option[ByteBuffer] = None

      def save(elem: T): SavingTool[T, M] = {
        println(elem)
        Pickle.intoBytes(elem)
        this
      }

      def load: M = {
        Unpickle[M].fromBytes(buf.get)
      }
    }
  }*/

  test("Test save Position") {
    val p: Position = Position(5,10)
    var buf = Pickle.intoBytes(p)
    assert(p==Unpickle[Position].fromBytes(buf))

    val p2 = Position(5,10)
    buf = Pickle.intoBytes(p)
    assert(p==Unpickle[Position].fromBytes(buf))
  }

  test("Test save LocationalField") {
    val l: LocationalField = LocationalField(1000, 1000, 5, Position(5,6))
    var buf = Pickle.intoBytes(l)
    assert(l==Unpickle[LocationalField].fromBytes(buf))
  }

  test("Test save LongTermMemory"){
    val l: LongTermMemory = LongTermMemory(MemoryType.HUNTING, LocationalField(1000, 1000, 5, Position(5,5)), 50)
    val m: AbstractMemoryMemento = l.serialize
    var buf = Pickle.intoBytes(m)
    val m2: LongTermMemoryMemento = Unpickle[AbstractMemoryMemento].fromBytes(buf) match {
      case l: LongTermMemoryMemento => l
      case _ => throw new IllegalStateException()}
    val l2 = LongTermMemory(m2)
    assert(m==l2.serialize)
  }

  test("Test save ShortTermMemory"){
    val s: ShortTermMemory = ShortTermMemory(MemoryType.HUNTING, LocationalField(1000, 1000, 5, Position(5,5)), 50)
    s.updateTime()
    val m: AbstractMemoryMemento = s.serialize
    var buf = Pickle.intoBytes(m)
    val m2: ShortTermMemoryMemento = Unpickle[AbstractMemoryMemento].fromBytes(buf) match {
      case s: ShortTermMemoryMemento => s
      case _ => throw new IllegalStateException()}
    val s2 = ShortTermMemory(m2)
    assert(m==s2.serialize)

    assert(s.score==s2.score)
    assert(s.elapsedTime==s2.elapsedTime)
  }

  test("Test save Neocortext") {
    val n = Neocortex()
    n.addMemory(LongTermMemory(MemoryType.HUNTING, LocationalField(1000, 1000, 5, Position(5,5)), 50))
    val m: NeocortexMemento = n.serialize
    var buf = Pickle.intoBytes(m)
    val m2: NeocortexMemento = Unpickle[NeocortexMemento].fromBytes(buf)
    val n2 = Neocortex(m2)
    assert(m==n2.serialize)

    assert(n.getMemeories(MemoryType.HUNTING).get.map(m => m.serialize) ==
    n2.getMemeories(MemoryType.HUNTING).get.map(m => m.serialize))
  }

  test("Test save Hippocampus") {
    val h: Hippocampus = Hippocampus(1000, 1000, 5)
    h.notifyEvent(MemoryType.HUNTING, Position(50, 80))
    h.notifyEvent(MemoryType.HUNTING, Position(10, 10))
    var m: HippocampusMemento = h.serialize
    var buf = Pickle.intoBytes(m)
    var m2: HippocampusMemento = Unpickle[HippocampusMemento].fromBytes(buf)
    var h2 = Hippocampus(m2)
    assert(m==h2.serialize)

    h.updateTime()
    h.startNewSearch(MemoryType.HUNTING)
    h.chooseNewMemory(Position(5,5))
    m = h.serialize
    buf = Pickle.intoBytes(m)
    m2 = Unpickle[HippocampusMemento].fromBytes(buf)
    h2 = Hippocampus(m2)
    assert(m==h2.serialize)
    assert(m.memorySearchComponent.get.memories==m2.memorySearchComponent.get.memories)
    val currentPosition: Position = Position(10, 10)
    assert(h.computeDirection(currentPosition)==h2.computeDirection(currentPosition))

    h.chooseNewMemory(Position(5,5))
    m = h.serialize
    buf = Pickle.intoBytes(m)
    m2 = Unpickle[HippocampusMemento].fromBytes(buf)
    h2 = Hippocampus(m2)
    assert(m==h2.serialize)
    assert(h.hasNewMemory==h2.hasNewMemory)
  }

  test("Test ReproductionComponent"){
    val r = ReproductionComponent()
    r.addMemory(LongTermMemory(MemoryType.HUNTING, LocationalField(1000, 1000, 5, Position(5,5)), 50))
    val m: NeocortexMemento = r.serialize
    var buf = Pickle.intoBytes(m)
    val m2: NeocortexMemento = Unpickle[NeocortexMemento].fromBytes(buf)
    val r2 = Neocortex(m2)
    assert(m==r2.serialize)
  }

//  test("Test save Entity") {
//    val e = Entity("improved", "e1")(ExecutionContext.Implicits.global)
////    e.addComponent()
//    val m: ImprovedEntityMemento = e.serialize match {
//      case i: ImprovedEntityMemento => i
//      case _ => throw new IllegalStateException()
//    }
//    var buf = Pickle.intoBytes(m)
//    val m2: ImprovedEntityMemento = Unpickle[ImprovedEntityMemento].fromBytes(buf)
//    val n2 = Entity(m2)
//    assert(m==n2.serialize)
//  }

}

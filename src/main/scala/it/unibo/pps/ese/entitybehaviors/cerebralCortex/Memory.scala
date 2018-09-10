package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.controller.saving.Savable
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType

object Memory {
  type Score = Double

  def apply(abstractMemoryMemento: AbstractMemoryMemento): Memory = abstractMemoryMemento match {
    case l: LongTermMemoryMemento => LongTermMemory(l)
    case s: ShortTermMemoryMemento => ShortTermMemory(s)
  }

  sealed trait Memory extends Savable[AbstractMemoryMemento] {
    def memoryType: MemoryType
    def locationalField: LocationalField
    def score: Score
    def score_=(score: Score): Unit
    def updateTime()
  }

  trait LongTermMemory extends Memory

  trait ShortTermMemory extends Memory {
    def elapsedTime: Int
  }

  private abstract class MemoryImpl(val memoryType: MemoryType, val locationalField: LocationalField, score: Score) extends Memory

  object LongTermMemory {
    def apply(memoryType: MemoryType, locationalField: LocationalField, score: Score): LongTermMemory =
      new LongTermMemoryImpl(memoryType, locationalField, score)

    def apply(longTermMemoryMemento: LongTermMemoryMemento): LongTermMemory =
      new LongTermMemoryImpl(MemoryType(longTermMemoryMemento.memoryType), longTermMemoryMemento.locationalField, longTermMemoryMemento.score)

    private class LongTermMemoryImpl(memoryType: MemoryType,
                                     locationalField: LocationalField,
                                     var score: Score) extends
      MemoryImpl(memoryType, locationalField, score) with LongTermMemory {

      val longTermMemoryDecadeValue = 1

      override def updateTime(): Unit = {
        score -= longTermMemoryDecadeValue
      }

      override def serialize: LongTermMemoryMemento = {
        LongTermMemoryMemento(memoryType.id, locationalField, score)
      }
    }

    implicit def shortTermMemorytoLongTermMemory(memoryType: MemoryType,shortTermMemory: ShortTermMemory): LongTermMemory = {
      LongTermMemory(memoryType, shortTermMemory.locationalField, shortTermMemory.score)
    }
  }

  object ShortTermMemory {
    def apply(memoryType: MemoryType, locationalField: LocationalField, score: Score): ShortTermMemory =
      new ShortTermMemoryImpl(memoryType, locationalField, score, None)

    def apply(shortTermMemoryMemento: ShortTermMemoryMemento): ShortTermMemory =
      new ShortTermMemoryImpl(MemoryType(shortTermMemoryMemento.memoryType), shortTermMemoryMemento.locationalField,
        shortTermMemoryMemento.score, Some(shortTermMemoryMemento))

    private class ShortTermMemoryImpl(memoryType: MemoryType,
                                      locationalField: LocationalField,
                                      var score: Score,
                                      shortTermMemoryMemento: Option[ShortTermMemoryMemento])
      extends MemoryImpl(memoryType, locationalField, score) with ShortTermMemory {

      var elapsedTime: Int = shortTermMemoryMemento match {
        case Some(m) => m.elapsedTime
        case _ => 0
      }

      override def updateTime(): Unit = elapsedTime+=1

      override def serialize: ShortTermMemoryMemento = {
        ShortTermMemoryMemento(memoryType.id, locationalField, score, elapsedTime)
      }
    }

    implicit def longTermMemorytoShortTermMemory(memoryType: MemoryType, longTermMemory: LongTermMemory): ShortTermMemory = {
      ShortTermMemory(memoryType, longTermMemory.locationalField, longTermMemory.score)
    }
  }

  sealed abstract class AbstractMemoryMemento

  case class LongTermMemoryMemento(memoryType: Int, locationalField: LocationalField, score: Score) extends AbstractMemoryMemento

  case class ShortTermMemoryMemento(memoryType: Int, locationalField: LocationalField, score: Score, elapsedTime: Int) extends AbstractMemoryMemento

}

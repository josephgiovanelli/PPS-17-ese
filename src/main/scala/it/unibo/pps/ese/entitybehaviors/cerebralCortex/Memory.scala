package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.MemoryType.MemoryType

object Memory {
  type Score = Double

  sealed trait Memory extends Serializable {
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

    private class LongTermMemoryImpl(memoryType: MemoryType, locationalField: LocationalField, var score: Score) extends
      MemoryImpl(memoryType, locationalField, score) with LongTermMemory {

      val longTermMemoryDecadeValue = 1

      override def updateTime(): Unit = {
        score -= longTermMemoryDecadeValue
      }
    }

    implicit def shortTermMemorytoLongTermMemory(memoryType: MemoryType, shortTermMemory: ShortTermMemory): LongTermMemory = {
      LongTermMemory(memoryType, shortTermMemory.locationalField, shortTermMemory.score)
    }
  }

  object ShortTermMemory {
    def apply(memoryType: MemoryType, locationalField: LocationalField, score: Score): ShortTermMemory =
      new ShortTermMemoryImpl(memoryType, locationalField, score)

    private class ShortTermMemoryImpl(memoryType: MemoryType, locationalField: LocationalField, var score: Score) extends
      MemoryImpl(memoryType, locationalField, score) with ShortTermMemory {

      var elapsedTime: Int = 0

      override def updateTime(): Unit = elapsedTime+=1
    }

    implicit def longTermMemorytoShortTermMemory(memoryType: MemoryType, longTermMemory: LongTermMemory): ShortTermMemory = {
      ShortTermMemory(memoryType, longTermMemory.locationalField, longTermMemory.score)
    }
  }


}

package it.unibo.pps.ese.entitybehaviors.cerebralCortex

object Memory {
  type Score = Double

  protected sealed trait Memory {
    def locationalField: LocationalField
    def score: Score
    def score_=(score: Score): Unit
  }

  trait LongTermMemory extends Memory

  trait ShortTermMemory extends Memory {
    def elapsedTime: Int
    def incrementElapsedTime()
  }

  private abstract class MemoryImpl(val locationalField: LocationalField, var score: Score) extends Memory

  object LongTermMemory {
    def apply(locationalField: LocationalField, score: Score): LongTermMemory = new LongTermMemoryImpl(locationalField, score)

    private class LongTermMemoryImpl(locationalField: LocationalField, score: Score) extends MemoryImpl(locationalField, score) with LongTermMemory

    implicit def shortTermMemorytoLongTermMemory(shortTermMemory: ShortTermMemory): LongTermMemory = {
      LongTermMemory(shortTermMemory.locationalField, shortTermMemory.score)
    }
  }

  object ShortTermMemory {
    def apply(locationalField: LocationalField, score: Score): ShortTermMemory = new ShortTermMemoryImpl(locationalField, score)

    private class ShortTermMemoryImpl(locationalField: LocationalField, score: Score) extends MemoryImpl(locationalField, score) with ShortTermMemory {

      var elapsedTime: Int = 0

      override def incrementElapsedTime(): Unit = elapsedTime+=1
    }

    implicit def longTermMemorytoShortTermMemory(longTermMemory: LongTermMemory): ShortTermMemory = {
      ShortTermMemory(longTermMemory.locationalField, longTermMemory.score)
    }
  }


}

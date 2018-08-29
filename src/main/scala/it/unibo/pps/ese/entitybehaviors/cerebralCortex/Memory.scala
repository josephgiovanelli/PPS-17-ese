package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Location.LocationalField
import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.Score

private trait Memory {
  def locationalField: LocationalField
  def score: Score
  def score_=(score: Score): Unit
}

trait LongTermMemory extends Memory

trait ShortTermMemory extends Memory {
  def elapsedTime: Int
  def incrementElapsedTime()
}

object Memory {
  type Score = Double

  object LongTermMemory {
    def apply(locationalField: LocationalField, score: Score): LongTermMemory = new LongTermMemoryImpl(locationalField, score)
  }

  object ShortTermMemory {
    def apply(locationalField: LocationalField, score: Score): ShortTermMemory = new ShortTermMemoryImpl(locationalField, score)
  }


}

private abstract class MemoryImpl(val locationalField: LocationalField, var score: Score) extends Memory

private class LongTermMemoryImpl(locationalField: LocationalField, score: Score) extends MemoryImpl(locationalField, score) with LongTermMemory

private class ShortTermMemoryImpl(locationalField: LocationalField, score: Score) extends MemoryImpl(locationalField, score) with ShortTermMemory {

  var elapsedTime: Int = 0

  override def incrementElapsedTime(): Unit = elapsedTime+=1
}
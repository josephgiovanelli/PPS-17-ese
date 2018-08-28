package it.unibo.pps.ese.entitybehaviors.cerebralCortex

import it.unibo.pps.ese.entitybehaviors.cerebralCortex.Memory.Score

trait Memory {
  def score: Score
  def score_=(score: Score): Unit
}

object Memory {
  type Score = Double

  def apply(locationalField: LocationalField, score: Score): Memory = new MemoryImpl(locationalField, score)
}

class MemoryImpl(locationalField: LocationalField, var score: Score) extends Memory

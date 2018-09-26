package it.unibo.pps.ese.model.components.animals.brain.cerebralcortex

import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.MemoryType

object Memory {
  type Score = Double

  /**
    * The common interface of the memories
    */
  sealed trait Memory {
    /**
      *
      * @return the `MemoryType` of this memory
      */
    def memoryType: MemoryType

    /**
      *
      * @return the `LocationalField` of this memory
      */
    def locationalField: LocationalField

    /**
      *
      * @return the `Score` of this memory
      */
    def score: Score

    /**
      * Sets the `Score` of this memory
      *
      * @param score the new `Score`
      */
    def score_=(score: Score): Unit

    /**
      * Updates the time of the memories, checking also if they will survive or not
      */
    def updateTime()
  }

  /**
    * A memory that lives for long time
    */
  trait LongTermMemory extends Memory

  /**
    * A memory that lives for short time
    */
  trait ShortTermMemory extends Memory {
    /**
      *
      * @return the age of this memory
      */
    def elapsedTime: Int
  }

  private abstract class AbstractMemory(val memoryType: MemoryType, val locationalField: LocationalField, score: Score) extends Memory

  object LongTermMemory {
    def apply(memoryType: MemoryType, locationalField: LocationalField, score: Score): LongTermMemory =
      new LongTermMemoryImpl(memoryType, locationalField, score)

    private class LongTermMemoryImpl(memoryType: MemoryType, locationalField: LocationalField, var score: Score) extends
      AbstractMemory(memoryType, locationalField, score) with LongTermMemory {

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
      AbstractMemory(memoryType, locationalField, score) with ShortTermMemory {

      var elapsedTime: Int = 0

      override def updateTime(): Unit = elapsedTime+=1
    }

    implicit def longTermMemorytoShortTermMemory(memoryType: MemoryType, longTermMemory: LongTermMemory): ShortTermMemory = {
      ShortTermMemory(memoryType, longTermMemory.locationalField, longTermMemory.score)
    }
  }


}

package it.unibo.pps.ese.view.history

sealed trait HistoryAggregator{
  def processLog(historyLog: HistoryLog):List[Log]
}
object HistoryAggregator {
  def apply(): HistoryAggregator = new HistoryAggregatorImpl()
  private class HistoryAggregatorImpl() extends HistoryAggregator {
    val alertThreshold:Long = 1000
    val genThreshold = 10
    var numGeneration:Long = 0
    var totalPopulation:Long = 0
    var oldPopulation:Long = 0L
    override def processLog(historyLog: HistoryLog): List[Log] = {
      val sumFunction:Map[String,Long]=>Long = m => m.values.sum

      numGeneration = numGeneration + 1
      oldPopulation = totalPopulation
      totalPopulation = totalPopulation +
        sumFunction(historyLog.bornRegistry)-
        sumFunction(historyLog.deadRegistry)

      val popLogs = for(i <-0L to  totalPopulation/alertThreshold if i>oldPopulation/alertThreshold ) yield
        Log("The World population has reached the value of "+alertThreshold*i,PopulationLog)
      val generationsLog = for(i <-0L to numGeneration/genThreshold if i>(numGeneration-1)/genThreshold) yield
        Log("The World has reached the "+numGeneration +"° Generation",GenerationLog)
      popLogs.toList ::: generationsLog.toList
    }
  }
}
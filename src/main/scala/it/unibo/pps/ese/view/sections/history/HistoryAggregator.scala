package it.unibo.pps.ese.view.sections.history
import scalaz._
import Scalaz._

/**
  * A utilities class to aggregate the facts occurred in all the ages and produce the respective logs
  */
sealed trait HistoryAggregator{
  def processLog(historyLog: HistoryLog):List[Log]
}

object HistoryAggregator {
  def apply(): HistoryAggregator = new HistoryAggregatorImpl()
  private class HistoryAggregatorImpl() extends HistoryAggregator {
    var popBySpecies:Map[String,Long] = Map()
    val alertThreshold:Long = 1000
    val genThreshold = 10
    var numGeneration:Long = 0
    var totalPopulation:Long = 0
    var oldPopulation:Long = 0L
    override def processLog(historyLog: HistoryLog): List[Log] = {
      //The total population, that is the old population plus the new born minus the dead entities
      popBySpecies =
          popBySpecies |+|
          historyLog.bornRegistry |+|
          historyLog.deadRegistry.mapValues(-_)

      numGeneration = numGeneration + 1
      oldPopulation = totalPopulation
      totalPopulation = popBySpecies.values.sum
      //Every alertThreshold( e.g 1000 ) produce a log with the reached value and the most populous species
      val popLogs = for{
        i <-0L to  totalPopulation/alertThreshold if i>oldPopulation/alertThreshold
        popLog = Log("The World population has reached the value of "+alertThreshold*i,PopulationLog)
        mostPopulous = mostPopulousSpecies
        dominantSpecies = Log(
          "The most populous species is the "+mostPopulous._1+" with "+mostPopulous._2+" unities",
          MostPopulousLog
        )
      }yield List(popLog,dominantSpecies)
      //Every genThreshold (e.g 10) produce a GenerationLog
      val generationsLog = for{
        i <-0L to numGeneration/genThreshold if i>(numGeneration-1)/genThreshold
      } yield Log("The World has reached the "+numGeneration +"° Generation",GenerationLog)

      Log("Era number "+numGeneration,NewEraLog)::popLogs.flatten.toList ::: generationsLog.toList
    }
    private def mostPopulousSpecies:(String,Long) = popBySpecies.toList.maxBy(_._2)
  }
}
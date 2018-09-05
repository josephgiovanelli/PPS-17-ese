package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.dataminer.{DataAggregator, DataMiner, DataSaver}
import it.unibo.pps.ese.genericworld.model.World

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

sealed trait SimulationLoop {
  def play(): Unit
  def pause(): Unit
  def dispose(): Unit
}

object SimulationLoop {

  def apply(model: World, period: FiniteDuration)
           (implicit executionContext: ExecutionContext): SimulationLoop = BaseSimulationLoop(model, period)

  private case class BaseSimulationLoop(model: World, period: FiniteDuration)
                                       (implicit executionContext: ExecutionContext) extends SimulationLoop {

    private[this] val timer = new java.util.Timer()
    private[this] var scheduledTask = None: Option[java.util.TimerTask]
    private[this] var era: Long = 0

    override def play(): Unit = {

      if (scheduledTask isDefined) throw new IllegalStateException("Loop already running")

      val task = new java.util.TimerTask {
        def run(): Unit = {

          DataAggregator ingestData (era, model entitiesState)
          era += 1

          val ret =
            for {
              b <- model.requireStateUpdate
              //c <- model.requireInfoUpdate
            } yield b

          Await.result(ret, Duration.Inf)

          val populationTrend = DataMiner(DataAggregator ingestedData) populationTrend()
          //val worldSpecies = DataMiner(DataAggregator ingestedData) worldSpecies()
          println(populationTrend)
          //println(worldSpecies)

          if (era == 10) {
            val tmp = (DataAggregator ingestedData) entitiesInEra  1
            tmp filter (x => x.structuralData.reign == "ANIMAL") take 1 foreach (x => {
              val y = (DataAggregator ingestedData) entityDynamicLog  x.id
              //println(y)

//              val originalData = (DataAggregator ingestedData) getAllDynamicLogs()
//              val saver = DataSaver()
//              val serialized = saver saveData("", originalData)
//              val deserialized = saver loadData serialized
//              println(deserialized)
            })
          }
        }
      }
      timer.scheduleAtFixedRate(task, 0, period.toMillis)
      scheduledTask = Some(task)
    }

    override def pause(): Unit = {
      this.scheduledTask.getOrElse(throw new IllegalStateException("No task defined")) cancel()
      this.scheduledTask = None
    }

    override def dispose(): Unit = {
      this.timer.cancel()
    }
  }
}



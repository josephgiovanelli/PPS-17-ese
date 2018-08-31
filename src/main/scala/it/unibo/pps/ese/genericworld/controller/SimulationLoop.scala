package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.dataminer.DataInterceptor
import it.unibo.pps.ese.genericworld.model.World

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

sealed trait SimulationLoop {
  def play(): Unit
  def pause(): Unit
  def dispose(): Unit
}

object SimulationLoop {

  def apply(model: World, period: FiniteDuration): SimulationLoop = BaseSimulationLoop(model, period)

  private case class BaseSimulationLoop(model: World, period: FiniteDuration) extends SimulationLoop {

    private[this] val timer = new java.util.Timer()
    private[this] var scheduledTask = None: Option[java.util.TimerTask]
    private[this] var era: Long = 0

    override def play(): Unit = {

      if (scheduledTask isDefined) throw new IllegalStateException("Loop already running")

      val task = new java.util.TimerTask {
        def run(): Unit = {

          era += 1

          val ret =
            for {
              b <- model.requireStateUpdate
              //c <- model.requireInfoUpdate
            } yield b

          Await.result(ret, Duration.Inf)
          DataInterceptor ingestData (era, model entitiesState)
          val data = DataInterceptor readData era
          if (era == 100) {
            val tmp = DataInterceptor readData 1
            tmp filter (x => x.structuralData.reign == "ANIMAL") take 1 foreach (x => {
              val y = DataInterceptor readData x.id
              println("ciao")
            })
            println(100)
          }
          println(data)
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



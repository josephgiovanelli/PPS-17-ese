package it.unibo.pps.ese.controller

import it.unibo.pps.ese.model.World

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

sealed trait SimulationLoop {
  def play(): Unit
  def pause(): Unit
  def dispose(): Unit
}

object SimulationLoop {

  def apply(model: World, period: Long): SimulationLoop = BaseSimulationLoop(model, period)

  private case class BaseSimulationLoop(model: World, period: Long) extends SimulationLoop {

    private[this] val timer = new java.util.Timer()
    private[this] var scheduledTask = None: Option[java.util.TimerTask]

    override def play(): Unit = this synchronized {

      if (scheduledTask isDefined) throw new IllegalStateException("Loop already running")

      val task = new java.util.TimerTask {
        def run(): Unit = {

          val ret =
            for {
              _ <- model.requireStateUpdate
              b <- model.requireInfoUpdate
            } yield b

          Await.result(ret, period millis )
        }
      }
      timer.schedule(task, 0, period)
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



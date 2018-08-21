package it.unibo.pps.ese.genericworld.controller

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

    override def play(): Unit = this synchronized {

      if (scheduledTask isDefined) throw new IllegalStateException("Loop already running")

      val task = new java.util.TimerTask {
        def run(): Unit = {

          val ret =
            for {
              b <- model.requireStateUpdate
              //c <- model.requireInfoUpdate
            } yield b

          try {
            Await.result(ret, period)
          } catch {
            case e: Exception => {
              println("ciao")
              println(e printStackTrace())
            }
          }
        }
      }
      timer.schedule(task, 0, period.toMillis)
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



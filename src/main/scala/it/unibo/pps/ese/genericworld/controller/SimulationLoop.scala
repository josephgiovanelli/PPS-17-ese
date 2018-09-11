package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.genericworld.model.{Entity, EntityBuilderHelpers, World}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

sealed trait SimulationLoop {
  def play(): Unit
  def pause(): Unit
  def dispose(): Unit
  def attachEraListener(listener: Long => Unit): Unit

  def addEntities(entities: Map[String, Int]): Unit
}

object SimulationLoop {

  def apply(model: World, period: FiniteDuration)
           (implicit executionContext: ExecutionContext): SimulationLoop = BaseSimulationLoop(model, period)

  private case class BaseSimulationLoop(model: World, period: FiniteDuration)
                                       (implicit executionContext: ExecutionContext) extends SimulationLoop {

    private[this] val _timer = new java.util.Timer()
    private[this] var _scheduledTask = None: Option[java.util.TimerTask]
    private[this] var _era: Long = 0
    private[this] var _eraListeners: Seq[Long => Unit] = Seq empty

    override def play(): Unit = {

      if (_scheduledTask isDefined) throw new IllegalStateException("Loop already running")

      val task = new java.util.TimerTask {
        def run(): Unit = {

          _era += 1

          println("Era " + _era + " computation started")

          val ret =
            for {
              b <- model.requireStateUpdate
              //c <- model.requireInfoUpdate
            } yield b

          Await.result(ret, Duration.Inf)

          println("Era " + _era + " computation finished")

          _eraListeners foreach(_(_era))
        }
      }
      _timer scheduleAtFixedRate(task, 0, period.toMillis)
      _scheduledTask = Some(task)
    }

    override def pause(): Unit = {
      _scheduledTask.getOrElse(throw new IllegalStateException("No task defined")) cancel()
      _scheduledTask = None
    }

    override def dispose(): Unit = {
      _timer cancel()
    }

    override def attachEraListener(listener: Long => Unit): Unit = _eraListeners = _eraListeners :+ listener

    override def addEntities(entities: Map[String, Int]): Unit =
      EntityBuilderHelpers.initializeEntities(entities, model.width, model.height).foreach(entity => model.addEntity(entity))
  }
}



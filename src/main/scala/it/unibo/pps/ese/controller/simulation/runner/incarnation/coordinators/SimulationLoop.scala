package it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators

import it.unibo.pps.ese.controller.simulation.runner.core.{Entity, World, WorldInfo}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

sealed trait SimulationLoop {
  def play(): Unit
  def pause(): Unit
  def dispose(): Unit
  def addEntities(entities: Seq[Entity]): Unit
  def worldInfo(): WorldInfo
  def attachEraListener(listener: Long => Unit): Unit
}

object SimulationLoop {

  def apply(model: World, period: FiniteDuration)
           (implicit executionContext: ExecutionContext): SimulationLoop = new BaseSimulationLoop(model, period)

  private class BaseSimulationLoop(model: World, period: FiniteDuration)
                                       (implicit executionContext: ExecutionContext) extends SimulationLoop {

    private[this] val _timer = new java.util.Timer()
    private[this] var _scheduledTask = None: Option[java.util.TimerTask]
    private[this] var _era: Long = 0
    private[this] var _eraListeners: Seq[Long => Unit] = Seq empty

    override def play(): Unit = {

      if (_scheduledTask isDefined) throw new IllegalStateException("Loop already running")
      val task = new java.util.TimerTask {
        def run(): Unit = {
          _eraListeners foreach(_(_era))
          Await.result(model.requireStateUpdate, Duration.Inf)
          _era += 1
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

    override def addEntities(entities: Seq[Entity]): Unit = entities.foreach(entity => model.addEntity(entity))

    override def worldInfo(): WorldInfo = model info()

    override def attachEraListener(listener: Long => Unit): Unit = _eraListeners = _eraListeners :+ listener

  }
}



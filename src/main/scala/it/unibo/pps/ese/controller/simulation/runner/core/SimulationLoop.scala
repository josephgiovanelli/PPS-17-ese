package it.unibo.pps.ese.controller.simulation.runner.core

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

/**
  *  Controls the simulation lifecycle
  */
sealed trait SimulationLoop {

  /**
    * Play the target simulation
    */
  def play(): Unit

  /**
    * Pause the target simulation
    */
  def pause(): Unit

  /**
    * Dispose the target simulation, freeing used resources
    */
  def dispose(): Unit

  /**
    * Add entities dynamically to the running simulation
    * @param entities Entities to add
    */
  def addEntities(entities: Seq[Entity]): Unit

  /**
    * Obtain info about the target simulation
    * @return Simulation's info
    */
  def worldInfo(): WorldInfo

  /**
    * Register a callback in order to be notified at every completed loop of the simulation's update cycle
    * @param listener The callback to register
    */
  def attachEraListener(listener: Long => Unit): Unit
}

object SimulationLoop {

  /**
    * @param model The simulation's model
    * @param period The update period
    * @param executionContext An execution context, required for async tasks
    * @return A SimulationLoop's instance
    */
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



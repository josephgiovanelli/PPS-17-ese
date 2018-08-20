package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.genericworld.model.World

sealed trait Controller {
  def initialize(frameRate: Int): Unit
  def play(): Unit
  def pause(): Unit
  def exit(): Unit
}

object Controller {

  def apply(world: World, view: View): Controller = BaseController(world, view)

  private case class BaseController(world: World, view:View) extends Controller {

    var simulationLoop = SimulationLoop(world, 15L)
    var stop = false
    var paused = true

    override def initialize(frameRate: Int): Unit =
      new Thread (() => {
        while(!stop) {
          normalizeFrameRate(() => {
            if (paused) this synchronized wait()
            view render (world entitiesState)
          }, frameRate)
        }
      }) start()

    override def play(): Unit = this synchronized {
      simulationLoop play()
      paused = false
      notify()
    }

    override def pause(): Unit = this synchronized {
      simulationLoop pause()
      paused = true
    }

    override def exit(): Unit = this synchronized {
      simulationLoop dispose()
      stop = true
      paused = true
      notify()
    }

    private def normalizeFrameRate(job: () => Unit, fps: Int): Unit = {
      val start = System.currentTimeMillis()
      job()
      val stop = System.currentTimeMillis()
      if (stop - start < 1000 / fps) this synchronized wait((1000 / fps) - (stop - start))
    }
  }
}

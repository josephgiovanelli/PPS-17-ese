package it.unibo.pps.ese.genericworld.controller

import it.unibo.pps.ese.genericworld.model.World

import scala.concurrent.duration.FiniteDuration

sealed trait Controller {
  def attachView(view: View, frameRate: Int): Unit
  def play(): Unit
  def pause(): Unit
  def exit(): Unit
}

object Controller {

  def apply(world: World, clockPeriod: FiniteDuration): Controller = BaseController(world, clockPeriod)

  private case class BaseController(world: World, clockPeriod: FiniteDuration) extends Controller {

    val simulationLoop = SimulationLoop(world, clockPeriod)
    var stop = false
    var paused = true

    override def attachView(view: View, frameRate: Int): Unit =
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
      if (stop - start < 1000 / fps) {
        Thread.sleep((1000 / fps) - (stop - start))
        //this synchronized wait((1000 / fps) - (stop - start))
      }
    }
  }
}

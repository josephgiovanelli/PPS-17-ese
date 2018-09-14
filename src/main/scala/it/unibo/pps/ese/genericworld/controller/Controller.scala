package it.unibo.pps.ese.genericworld.controller

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.Paths

import it.unibo.pps.ese.genericworld.model.{Entity, EntityState, World}
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.View

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

sealed trait Controller {
  def attachView(view: View, frameRate: Int): Unit
  def manage: ManageableController
}

trait ManageableController {
  def play(): Unit
  def pause(): Unit
  def exit(): Unit
  def entityData(id: String): Option[EntityState]
  def saveWorld()
}

object Controller {

  def apply(world: World, clockPeriod: FiniteDuration)
           (implicit executionContext: ExecutionContext): Controller = BaseController(world, clockPeriod)

  private case class BaseController(world: World, clockPeriod: FiniteDuration)
                                   (implicit executionContext: ExecutionContext) extends Controller with ManageableController {

    val simulationLoop = SimulationLoop(world, clockPeriod)
    var stop = false
    var paused = true

    override def attachView(view: View, frameRate: Int): Unit = {
      import ViewHelpers.{ManageableObserver, toViewData}
      view addObserver this
      new Thread (() => {
        while(!stop) {
          normalizeFrameRate(() => {
            if (paused) this synchronized wait()
            view updateWorld (0, world entitiesState)
          }, frameRate)
        }
      }) start()
    }

    override def manage: ManageableController = this

    override def entityData(id: String): Option[EntityState] = (world entitiesState).find(x => x.entityId == id)

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

    override def saveWorld(): Unit = {
      new Thread (() => {
        println("Saving started")

        val fileName = "/World"
        val p = Paths.get(Controller.getClass.getResource("/it/unibo/pps/ese/controller/saves").toURI).toString + fileName

        var oos = new ObjectOutputStream(new FileOutputStream(p))
        oos.writeObject(world.entities)
        oos.close()


        val fileName2 = "/GeneticsSimulator"
        val p2 = Paths.get(Controller.getClass.getResource("/it/unibo/pps/ese/controller/saves").toURI).toString + fileName2

        oos = new ObjectOutputStream(new FileOutputStream(p2))
        oos.writeObject(GeneticsSimulator)
        oos.close()

        println("Saving ended")

      }).start()
    }
  }
}

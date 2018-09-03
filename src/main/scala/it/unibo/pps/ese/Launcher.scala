package it.unibo.pps.ese

import it.unibo.pps.ese.genericworld.controller.Controller
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation
import it.unibo.pps.ese.genericworld.model.{SimulationBuilder, World}
import it.unibo.pps.ese.view.View
import scalafx.application.JFXApp

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Launcher extends JFXApp {
  require(parameters.raw.size == 1, "Application requires an input path corresponding to a simulation's config file")
  //val world: World = WorldBuilder buildWorldFromSimulationData (parameters.raw.head, 500, 500)
  //val controller = Controller(world, clockPeriod = 250 millis)
  val controller: Controller =
  new SimulationBuilder[EmptySimulation] dimension(500, 500) data "it/unibo/pps/ese/controller/loader/Simulation.yml" build
  val view = View()
  stage = view
  controller attachView (view, frameRate = 30)
  controller.manage.play()

}
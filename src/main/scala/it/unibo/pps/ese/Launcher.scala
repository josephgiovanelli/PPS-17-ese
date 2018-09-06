package it.unibo.pps.ese

import it.unibo.pps.ese.genericworld.TestLauncher.stage
import it.unibo.pps.ese.genericworld.controller.Controller
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation
import it.unibo.pps.ese.genericworld.model.{SimulationBuilder, World}
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.View
import scalafx.application.JFXApp
import scalafx.stage.WindowEvent

import scalafx.Includes._
import scala.concurrent.ExecutionContext.Implicits.global

object Launcher extends JFXApp {
  require(parameters.raw.size == 1, "Application requires an input path corresponding to a simulation's config file")

  val controller: Controller =
  new SimulationBuilder[EmptySimulation] dimension(500, 500) data parameters.raw.head build

  val view = View(GeneticsSimulator)
  stage = view
  controller attachView (view, frameRate = 30)
  controller.manage.play()

  stage.setOnCloseRequest((e: WindowEvent) => {
    controller.manage.exit()
  })

}
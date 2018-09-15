package it.unibo.pps.ese

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.genericworld.controller.Controller
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation
import it.unibo.pps.ese.genericworld.model.SimulationBuilder
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.View
import scalafx.application.JFXApp
import scalafx.stage.WindowEvent
import scalafx.Includes._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Launcher extends JFXApp {
  require(parameters.raw.size == 1, "Application requires an input path corresponding to a simulation's config file")

  YamlLoader.loadSimulation(parameters.raw.head) match {
    case data: CompleteSimulationData =>
      val controller: Controller =
        new SimulationBuilder[EmptySimulation]
          .dimension(500, 500)
          .data(data)
          .build

      val view = View(GeneticsSimulator)
      stage = view
      controller attachView (view, frameRate = 30)
      controller.manage.play()

      stage.setOnCloseRequest((e: WindowEvent) => {
        controller.manage.exit()
      })
    case _ =>
      throw new IllegalStateException()
  }

//  YamlLoader.loadSimulation(parameters.raw.head) match {
//    case data: CompleteSimulationData =>
//      val controller: Controller =
//        new SimulationBuilder[EmptySimulation]
//          .dimension(500, 500)
//          .data(data)
//          .build
//
//      val view = View(GeneticsSimulator)
//      stage = view
//      controller attachView (view, frameRate = 30)
//      controller.manage.play()
//
//      stage.setOnCloseRequest((e: WindowEvent) => {
//        controller.manage.exit()
//      })
//    case _ =>
//      throw new IllegalArgumentException
//  }

}
package it.unibo.pps.ese.genericworld

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.controller.util.io.File
import it.unibo.pps.ese.genericworld.controller.Controller
import it.unibo.pps.ese.genericworld.model._
import it.unibo.pps.ese.genericworld.model.SimulationBuilder.Simulation.EmptySimulation
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.View
import org.kaikikm.threadresloader.ResourceLoader
import scalafx.application.JFXApp
import scalafx.stage.WindowEvent

import scalafx.Includes._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object TestLauncher extends JFXApp {

  YamlLoader.loadCompleteSimulation(File(ResourceLoader.getResource("it/unibo/pps/ese/controller/loader/Simulation.yml"))) match {
    case Success(data) =>
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
    case Failure(exception) =>
      throw exception
  }
}

package it.unibo.pps.ese

import it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators.Controller
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.core.ViewLauncher

import scalafx.application.JFXApp
import scala.concurrent.ExecutionContext.Implicits.global

object Launcher extends JFXApp {
  ViewLauncher(GeneticsSimulator, Controller())
}
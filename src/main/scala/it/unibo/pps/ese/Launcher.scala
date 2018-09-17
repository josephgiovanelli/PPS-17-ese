package it.unibo.pps.ese

import it.unibo.pps.ese.genericworld.controller.Controller
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.View
import scalafx.application.JFXApp

import scala.concurrent.ExecutionContext.Implicits.global

object Launcher extends JFXApp {

  View(GeneticsSimulator, Controller())

}
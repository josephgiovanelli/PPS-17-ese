package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.View
import scalafx.application.JFXApp

import scala.concurrent.ExecutionContext.Implicits.global

object TestConfigurationLauncher extends JFXApp {
  val view = View(GeneticsSimulator)
  stage = view
}

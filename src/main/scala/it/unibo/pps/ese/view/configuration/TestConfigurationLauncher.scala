package it.unibo.pps.ese.view.configuration

import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.View
import scalafx.application.JFXApp

object TestConfigurationLauncher extends JFXApp {
  val view = View(GeneticsSimulator)
  stage = view
}

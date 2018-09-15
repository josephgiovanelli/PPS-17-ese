package it.unibo.pps.ese.view

import it.unibo.pps.ese.genetics.GeneticsSimulator
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene

import scala.concurrent.ExecutionContext.Implicits.global

object WorldPrefernces {
  val worldWidth: Int = 500
  val worldHeigth: Int = 500
}

object ViewLauncher extends JFXApp {

  val view: View = View(GeneticsSimulator)
  stage = view

  //val controller: Controller = new Controller(view)
}

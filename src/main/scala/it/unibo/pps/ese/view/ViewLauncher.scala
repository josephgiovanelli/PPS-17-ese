package it.unibo.pps.ese.view

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene

object WorldPrefernces {
  val worldWidth: Int = 500
  val worldHeigth: Int = 500
}

object ViewLauncher extends JFXApp {

  val view: View = View()
  stage = view

  //val controller: Controller = new Controller(view)
}
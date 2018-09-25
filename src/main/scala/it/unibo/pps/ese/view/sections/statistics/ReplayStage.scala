package it.unibo.pps.ese.view.sections.statistics

import it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators.ReplayController
import scalafx.scene.Scene
import scalafx.stage.Stage

/**
  * Window where chosen entity's replay data are shown
  * @param replayTarget The entity identifier
  * @param replayController The ReplayController from which fetch data
  */
class ReplayStage(replayTarget: String, replayController: ReplayController) extends Stage {

  outer =>
  title = "Replay entity's " + replayTarget + " life"

  val replayPane = ReplayPane()
  replayController initialize replayTarget
  replayController attachView replayPane

  scene = new Scene {
    root = replayPane
  }

  onCloseRequest = _ => replayController dispose()
}

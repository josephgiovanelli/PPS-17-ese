package it.unibo.pps.ese.view.sections.statistics

import it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators.ReplayController
import scalafx.scene.Scene
import scalafx.stage.Stage

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

package it.unibo.pps.ese.view.start

import scalafx.application.JFXApp
import scalafx.scene.Group

object TestStartMenu extends JFXApp{ app=>

  val root = new Group
  stage = new JFXApp.PrimaryStage {
    scene = StartMenuView(null)
  }

  stage.show()
}

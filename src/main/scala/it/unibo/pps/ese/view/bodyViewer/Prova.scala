package it.unibo.pps.ese.view.bodyViewer


import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.layout.Pane
import scalafx.scene.{Group, PerspectiveCamera, Scene, SceneAntialiasing}
import scalafx.scene.paint.Color
import scalafx.scene.shape.CullFace

object Prova extends JFXApp{ app=>

  val root = new Group()
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(root, 1400, 800, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {
      fill = Color.Gray
      title = "Molecule Sample Application"
    }
  }
  val bodyPane = BodyPane()
  val pane = new Pane()
  pane.children += bodyPane
  root.children += pane

  stage.show()
}

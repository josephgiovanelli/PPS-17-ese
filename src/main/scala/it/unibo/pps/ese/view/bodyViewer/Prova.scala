package it.unibo.pps.ese.view.bodyViewer


import scalafx.Includes._
import org.fxyz3d.shapes.primitives.{CapsuleMesh, SpringMesh}
import org.fxyz3d.utils.CameraTransformer
import scalafx.application.JFXApp
import scalafx.scene.{Group, PerspectiveCamera, Scene, SceneAntialiasing}
import scalafx.scene.paint.Color
import scalafx.scene.shape.CullFace

object Prova extends JFXApp{ app=>

  val root = new Group()
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(root, 1200, 800, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {
      fill = Color.Gray
      title = "Molecule Sample Application"
    }
  }
  val bodyPane = BodyPane()
  root.children += bodyPane

  stage.show()
}

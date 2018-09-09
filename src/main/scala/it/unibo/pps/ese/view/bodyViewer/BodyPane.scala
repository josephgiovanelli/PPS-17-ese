package it.unibo.pps.ese.view.bodyViewer

import javafx.scene.text.{Font, Text}
import scalafx.Includes._
import scalafx.scene._
import scalafx.geometry.Insets
import scalafx.scene.control.Slider
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.transform.Rotate
import scalafx.scene.shape._
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.text.TextAlignment
import it.unibo.pps.ese.view.speciesdetails.TextUtilities._
import org.fxyz3d.shapes.Capsule
import org.fxyz3d.shapes.primitives.CapsuleMesh
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.{Image, ImageView}
sealed trait BodyPane extends Pane{
}
object BodyPane {
  def apply():BodyPane= new BodyPaneImpl()
  private[this] class BodyPaneImpl() extends BodyPane {
    prefWidth = 1000
    prefHeight = 1000
    background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))
    val brain = new Image("it.unibo.pps.ese.view/Common/brain.png")
    val eyes = new Image("it.unibo.pps.ese.view/Common/eyesNormal.png")
    val digestive = new Image("it.unibo.pps.ese.view/Man/digManN.png")

    val canvas = new Canvas(1000,800)
    val gc = canvas.graphicsContext2D
    gc.fill = Color.color(0.2, 0.2, 0.2, 1.0)
    val root = new Group()
    children += root
    root.children += canvas
    gc.drawImage(brain,350,0)
    gc.drawImage(eyes,385,80)
    gc.drawImage(digestive,330,200)
  }
}

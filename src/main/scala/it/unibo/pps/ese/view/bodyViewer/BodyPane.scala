package it.unibo.pps.ese.view.bodyViewer

import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import it.unibo.pps.ese.utils.ScalableImage
import it.unibo.pps.ese.view._
import javafx.scene.text.{Font, Text}
import scalafx.Includes._
import scalafx.scene._
import scalafx.geometry.Insets
import scalafx.scene.paint.Color
import scalafx.scene.layout._
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
sealed trait BodyPane extends Pane{
}
object BodyPane {
  def apply():BodyPane= new BodyPaneImpl()
  private[this] class BodyPaneImpl() extends BodyPane {
//    prefWidth = 1200
//    prefHeight = 900
    background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))
//      background = new Background(Array(new BackgroundFill(Color.White, CornerRadii.Empty, Insets.Empty)))
    val brain:Image = ScalableImage("it.unibo.pps.ese.view/Common/brain.png")
    val eyes:Image = ScalableImage("it.unibo.pps.ese.view/Common/eyesNormal.png")
    val digestive:Image = ScalableImage("it.unibo.pps.ese.view/Man/digManN.png")
//    val canvas = new Canvas(600,900)
//    val gc = canvas.graphicsContext2D
//    gc.fill = Color.color(0.2, 0.2, 0.2, 1.0)
//    gc.fillRect(0,0,600,800)
////    content = root
//    gc.fill = Color.White
//    val pad:Double = 150.0
//    gc.fillOval(0,0,550,800)
//    gc.drawImage(digestive,4+pad,200)
//    gc.drawImage(brain,22+pad,10)
//    gc.drawImage(eyes,55+pad,88.5)
//    val animal = FemaleAnimalRepresentation()
      val animal = MaleAnimalRepresentation()
//    Thread.sleep(1000)
//    canvas = femaleRepresentation.setBrainStatus(HippoCampusActive(Eating))
//    Thread.sleep(1000)
//    canvas = femaleRepresentation.setBrainStatus(HippoCampusDisabled)
//    Thread.sleep(1000)
//    femaleRepresentation.setEmbryoStatus(EmbryoStatus.primal)
//    canvas = femaleRepresentation.drawRepresentation
//    Thread.sleep(1000)
//    femaleRepresentation.setEmbryoStatus(EmbryoStatus.mid)
//    canvas = femaleRepresentation.drawRepresentation
//    Thread.sleep(1000)
//    animal.setEmbryoStatus(EmbryoStatus.advanced)
    var canvas = animal.drawRepresentation
    val root = new Group()
    children +=root
    root.children += canvas
    canvas.translateX = 400

  }

}

package it.unibo.pps.ese.view.bodyViewer

import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import it.unibo.pps.ese.utils.ScalableImage
import it.unibo.pps.ese.view._
import javafx.scene.text.{Font, Text}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene._
import scalafx.geometry.Insets
import scalafx.scene.paint.Color
import scalafx.scene.layout._
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
sealed trait BodyPane extends Pane{
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit
}
object BodyPane {
  def apply():BodyPane= new BodyPaneImpl()
  private[this] class BodyPaneImpl() extends BodyPane {
    background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))
    var oldAnimalStatus:Option[AnimalInternalStatus] = None
    def isAOldState(animalInternalStatus: AnimalInternalStatus):Boolean
    = oldAnimalStatus.isDefined match {
      case true=>
        (oldAnimalStatus.get,animalInternalStatus) match {
          case (male:MaleInternalStatus,maleNew:MaleInternalStatus) =>
            oldAnimalStatus = Some(animalInternalStatus)
            male==maleNew
          case (female:FemaleInternalStatus,femaleNew:FemaleInternalStatus) =>
            oldAnimalStatus = Some(animalInternalStatus)
            female==femaleNew
          case (_,_) => false
        }
      case _=>
        oldAnimalStatus = Some(animalInternalStatus)
        false
    }
    val root = new Group()
    children +=root
    updateAnimalInternalStatus(FemaleInternalStatus(HippoCampusDisabled,EyesDisabled,NotReproducing,Digesting,None))
    override def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus): Unit = {
      isAOldState(animalInternalStatus) match {
        case false=>
          val newCanvas = animalInternalStatus match {
            case FemaleInternalStatus(brain,eyes,reproductive,digestive,fetus)=> {
              println("Femmina",brain,eyes,reproductive,digestive,fetus)
              val female:FemaleAnimalRepresentation = FemaleAnimalRepresentation()
              if (fetus.isDefined) female.setEmbryoStatus(fetus.get)
              female.setBrainStatus(brain)
              female.setEyesStatus(eyes)
              female.setDigestiveSystemStatus(digestive)
              female.setReproductiveSystemStatus(reproductive)
            }
            case MaleInternalStatus(brain,eyes,reproductive,digestive)=> {
              println("MAschio",brain,eyes,reproductive,digestive)
              val male:MaleAnimalRepresentation = MaleAnimalRepresentation()
              male.setBrainStatus(brain)
              male.setEyesStatus(eyes)
              male.setDigestiveSystemStatus(digestive)
              male.setReproductiveSystemStatus(reproductive)
            }
          }
          Platform.runLater{
            ()->{
              root.children.clear()
              root.children += newCanvas
              newCanvas.translateX = 400
            }
          }
        case true=>
      }
//      def configureAnimalRepresentation(
//                                         brain: BrainStatus,
//                                         eyes:EyesStatus,
//                                         digestive:DigestiveSystemStatus,
//                                         reproductive:ReproductiveApparatusStatus
//                                       ):Canvas ={
//
//      }

    }
  }

}

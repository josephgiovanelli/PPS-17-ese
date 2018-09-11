package it.unibo.pps.ese.view.bodyViewer
import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import it.unibo.pps.ese.utils.ScalableImage
import it.unibo.pps.ese.view._
import javafx.scene.paint.ImagePattern
import javafx.scene.text.{Font, Text}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene._
import scalafx.geometry.Insets
import scalafx.scene.paint.Color
import javafx.scene.layout.{Border, BorderStroke, BorderStrokeStyle}
import javafx.scene.shape.{HLineTo, MoveTo, Path, VLineTo}
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
import scalafx.scene.layout._
sealed trait BodyPane extends Pane{
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit
}
object BodyPane {
  def apply():BodyPane= new BodyPaneImpl()
  private[this] class BodyPaneImpl() extends BodyPane {
//    background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))
    val back:Image = new Image("it.unibo.pps.ese.view/backO3.jpg")

    background = new Background(Array(new BackgroundFill(new ImagePattern(back),CornerRadii.Empty, Insets.Empty)))
    val root = new Group()
    val canvasGroup = new Group()
    children +=root

    val vBox = new VBox(10)
    val hBox1 = OrganDescriptionBox()
    val hBox2 = OrganDescriptionBox()
    val hBox3 = OrganDescriptionBox()
    vBox.children += hBox1
    vBox.children += hBox2
    vBox.children += hBox3
    vBox.translateX = 10
    root.children +=vBox
    root.children += canvasGroup
    private def updateBoxes(animalInternalStatus: AnimalInternalStatus): Unit ={
      hBox1.setText(
        OrganInfoPrinter.getHeadText(
          animalInternalStatus.brainStatus,
          animalInternalStatus.eyesStatus)
      )
      hBox2.setText(
        OrganInfoPrinter.getDigestiveSystemStatus(animalInternalStatus.digestiveSystemStatus)
      )
      animalInternalStatus match {
        case FemaleInternalStatus(brain, eyes, reproductive, digestive, fetus)=>
          hBox3.setText(OrganInfoPrinter.getReproductiveSystemStatus(reproductive,fetus))
        case MaleInternalStatus(brain, eyes, reproductive, digestive) =>
          hBox3.setText(OrganInfoPrinter.getReproductiveSystemStatus(reproductive,None))
      }
    }
    override def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus): Unit = {
      AnimalStatusUtilities.isAOldState(animalInternalStatus) match {
        case false =>{
          updateBoxes(animalInternalStatus)
          val newCanvas = animalInternalStatus match {
            case FemaleInternalStatus(brain, eyes, reproductive, digestive, fetus) => {
              println("Femmina", brain, eyes, reproductive, digestive, fetus)
              val female: FemaleAnimalRepresentation = FemaleAnimalRepresentation()
              if (fetus.isDefined) female.setEmbryoStatus(fetus.get)
              female.setBrainStatus(brain)
              female.setEyesStatus(eyes)
              female.setDigestiveSystemStatus(digestive)
              female.setReproductiveSystemStatus(reproductive)
            }
            case MaleInternalStatus(brain, eyes, reproductive, digestive) => {
              println("MAschio", brain, eyes, reproductive, digestive)
              val male: MaleAnimalRepresentation = MaleAnimalRepresentation()
              male.setBrainStatus(brain)
              male.setEyesStatus(eyes)
              male.setDigestiveSystemStatus(digestive)
              male.setReproductiveSystemStatus(reproductive)
            }
          }
          Platform.runLater {
            () -> {
              canvasGroup.children.clear()
              canvasGroup.children += newCanvas
                            newCanvas.translateX = 300
            }
          }
        }
        case true=>
      }

    }
  }
  private object OrganInfoPrinter{
    def getHeadText(brainStatus: BrainStatus,eyesStatus: EyesStatus):String
    =(brainStatus,eyesStatus) match {
      case (HippoCampusActive(r),EyesDisabled) =>
        "I can't see anything and I want to "+r+" so i am using my memory"
      case (HippoCampusDisabled,EyesActive(r)) =>
        "I want to "+r+" and I see a entity that can help me"
      case (HippoCampusDisabled,EyesDisabled)=>
        "I have nothing to do"
      case _=>""
    }
    def getDigestiveSystemStatus(digestiveSystemStatus: DigestiveSystemStatus):String=
      digestiveSystemStatus match {
        case Digesting => "What a good meal, I'm digesting..."
        case NotDigesting => "I have nothing in my stomach..."
      }
    def getReproductiveSystemStatus(
                                    reproductiveApparatusStatus: ReproductiveApparatusStatus,
                                    embryoStatus: Option[EmbryoStatus.Value]):String=
      (reproductiveApparatusStatus,embryoStatus) match {
        case (Reproducing,None) => "I found the right partner for me so I will copulate"
        case (NotReproducing,None) => "I have not found the right partner yet"
        case (NotReproducing,Some(v)) => "Oh...I'm pregnant"
        case _=>"No Description"
      }
  }
  private object AnimalStatusUtilities{
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
          case (_,_) =>
            oldAnimalStatus = Some(animalInternalStatus)
            false
        }
      case _=>
        oldAnimalStatus = Some(animalInternalStatus)
        false
    }
  }

}

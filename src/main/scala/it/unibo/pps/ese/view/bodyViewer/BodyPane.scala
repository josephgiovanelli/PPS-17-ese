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
import scalafx.scene.control.ScrollPane
import scalafx.scene.image.Image
import scalafx.scene.layout._
sealed trait BodyPane extends ScrollPane{
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit
}
object BodyPane {
  def apply():BodyPane= new BodyPaneImpl()
  private[this] class BodyPaneImpl() extends BodyPane {
    val back:Image = new Image("it.unibo.pps.ese.view/backO3.jpg")
    val canvasGroup = new Group()
    val root = new Pane()
    root.prefHeight = 850
    root.prefWidth = 1400
    content = root
    root.background = new Background(Array(new BackgroundFill(new ImagePattern(back),CornerRadii.Empty, Insets.Empty)))
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
        r match {
          case Evaluating =>
            "I can not see anything in my field" +
            " of vision and I'm thinking about what to do"
          case Coupling =>
            "I can not find a partner in my field of vision " +
              "but I'm trying to remember where I saw one"
          case Eating =>
            "I can not find a source of food in my field of vision " +
              "but I'm trying to remember where I saw one"
        }
      case (HippoCampusDisabled,EyesActive(r)) =>
        r match {
          case Evaluating =>
            "I'm looking around to decide what to do"
          case Coupling =>
            "I would like to reproduce myself and I have just identified a possible partner, great!"
          case Eating =>
            "I'm hungry and I just spotted something to feed myself"
        }
      case (HippoCampusDisabled,EyesDisabled)=>
        "I have nothing to do..."
      case _=>""
    }
    def getDigestiveSystemStatus(digestiveSystemStatus: DigestiveSystemStatus):String=
      digestiveSystemStatus match {
        case Digesting => "What a good meal, I'm digesting..."
        case NotDigesting => "I have nothing to digest..."
      }
    def getReproductiveSystemStatus(
                                    reproductiveApparatusStatus: ReproductiveApparatusStatus,
                                    embryoStatus: Option[EmbryoStatus.Value]):String=
      (reproductiveApparatusStatus,embryoStatus) match {
        case (Reproducing,None) => "I found the right partner for me so I will copulate"
        case (NotReproducing,None) => "I have not found the right partner yet"
        case (NotReproducing,Some(v)) =>
          val embryoText = v match {
            case EmbryoStatus.primal => " and my gestation is a the beginning"
            case EmbryoStatus.mid => " and my gestation is half-done"
            case EmbryoStatus.advanced => " and my gestation is a almost finished"
          }
          "Oh...I'm pregnant"+embryoText
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

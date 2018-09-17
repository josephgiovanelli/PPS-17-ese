package it.unibo.pps.ese.view.bodyViewer
import javafx.scene.paint.ImagePattern
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene._
import scalafx.geometry.Insets
import scalafx.scene.control.ScrollPane
import scalafx.scene.image.Image
import scalafx.scene.layout._

/**
  * A [[ScrollPane]] with the visual representation of the internal dynamics of an animal
  */
sealed trait BodyPane extends ScrollPane{
  /**
    * To update the internal status of an animal
    * @param animalInternalStatus
    *                             The new [[AnimalInternalStatus]]
    */
  def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus):Unit

  /**
    * To cancel the representation
    */
  def clearStatus():Unit
}

object BodyPane {
  def apply():BodyPane= new BodyPaneImpl()
  private[this] class BodyPaneImpl() extends BodyPane {
    val back:Image = new Image("it.unibo.pps.ese.view/backgrounds/backBig.jpg")
    val canvasGroup = new Group()
    val root = new Pane()
    root.prefHeight = 850
    root.prefWidth = 1600
    content = root
    root.background = new Background(Array(new BackgroundFill(new ImagePattern(back),CornerRadii.Empty, Insets.Empty)))
    val vBox = new VBox(10)
    val headBox:OrganDescriptionBox = OrganDescriptionBox()
    val digestiveBox:OrganDescriptionBox = OrganDescriptionBox()
    val reproductionBox:OrganDescriptionBox = OrganDescriptionBox()
    vBox.children += headBox
    vBox.children += digestiveBox
    vBox.children += reproductionBox
    vBox.translateX = 10
    root.children +=vBox
    root.children += canvasGroup

    /**
      * To update all the boxes with the description of the internal dynamic of the selected animal
      * @param animalInternalStatus
      */
    private def updateBoxes(animalInternalStatus: AnimalInternalStatus): Unit ={
      headBox.setText(
        OrganInfoPrinter.getHeadText(
          animalInternalStatus.brainStatus,
          animalInternalStatus.eyesStatus)
      )
      digestiveBox.setText(
        OrganInfoPrinter.getDigestiveSystemStatus(animalInternalStatus.digestiveSystemStatus)
      )
      animalInternalStatus match {
        case FemaleInternalStatus(brain, eyes, reproductive, digestive, fetus)=>
          reproductionBox.setText(OrganInfoPrinter.getReproductiveSystemStatus(reproductive,fetus))
        case MaleInternalStatus(brain, eyes, reproductive, digestive) =>
          reproductionBox.setText(OrganInfoPrinter.getReproductiveSystemStatus(reproductive,None))
      }
    }

    override def updateAnimalInternalStatus(animalInternalStatus: AnimalInternalStatus): Unit = {
      AnimalStatusUtilities.isAOldState(animalInternalStatus) match {
        case false =>{
          updateBoxes(animalInternalStatus)
          val newCanvas = animalInternalStatus match {
            case FemaleInternalStatus(brain, eyes, reproductive, digestive, fetus) => {
              val female: FemaleAnimalRepresentation = FemaleAnimalRepresentation()
              if (fetus.isDefined) female.setEmbryoStatus(fetus.get)
              female.setBrainStatus(brain)
              female.setEyesStatus(eyes)
              female.setDigestiveSystemStatus(digestive)
              female.setReproductiveSystemStatus(reproductive)
            }
            case MaleInternalStatus(brain, eyes, reproductive, digestive) => {
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

    override def clearStatus(): Unit = {
      canvasGroup.children.clear()
      AnimalStatusUtilities.oldAnimalStatus = None
      List(headBox,digestiveBox,reproductionBox).foreach(_.clearText())
    }
  }

  /**
    * Utility object that maintain the previous status of the animal, in order to avoid the drawing of the same status
    */
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

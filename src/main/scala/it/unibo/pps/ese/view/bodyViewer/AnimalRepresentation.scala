package it.unibo.pps.ese.view.bodyViewer

import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
import scalafx.scene.paint.Color

/**
  * Abstract implementation of the Visual Representation of the animal's internal state
  */
sealed abstract class AnimalRepresentation{
  /**
    * Brain representation
    */
  protected val brain: Image = BodyImages.brain
  /**
    * Activated Brain representation
    */
  protected val activatedBrain: Image = BodyImages.activatedBrain
  /**
    * Eyes representation
    */
  protected val eyes: Image = BodyImages.eyes
  /**
    * Activated eyes  representation
    */
  protected val activatedEyes: Image = BodyImages.activatedEyes
  /**
    * Digestive system representation
    */
  protected def digestiveSystem:Image
  /**
    * Activated Digestive system representation
    */
  protected def digestiveSystemActivated:Image
  /**
    * Activated Reproductive system representation
    */
  protected val reproductiveSystemActivated:Image
  protected var actualBrain:Image = brain
  protected var actualEyes:Image = eyes
  protected var actualDigestiveSystem:Image=digestiveSystem

  /**
    * Method to obtain the current animal representation drawn in a Canvas
    * @return
    *         A [[Canvas]] with the current animal representation
    */
  def drawRepresentation:Canvas = {
    val width = 300
    val height = 900
    val canvas = new Canvas(width,height)
    val gc = canvas.graphicsContext2D
    gc.fill = Color.Transparent
    gc.fillRect(0,0,width,height)
    val pad:Double = 50.0
    gc.drawImage(actualDigestiveSystem,2+pad,200)
    gc.drawImage(actualBrain,22+pad,10)
    gc.drawImage(actualEyes,55+pad,88.5)
    canvas
  }

  /**
    * Return a [[Canvas]] with the animal representation after setting the brain status
    * @param brainStatus
    *                    The [[BrainStatus]] to set
    * @return
    *         The [[Canvas]] after setting the brain status
    */
  def setBrainStatus(brainStatus: BrainStatus):Canvas = {
    actualBrain = brainStatus match {
      case HippoCampusActive(r)=> activatedBrain
      case HippoCampusDisabled => brain
    }
    drawRepresentation
  }

  /**
    * Return a [[Canvas]] with the animal representation after setting the eyes status
    * @param eyesStatus
    *                   The [[EyesStatus]] to set
    * @return
    *         The [[Canvas]] after setting the eyes status
    */
  def setEyesStatus(eyesStatus: EyesStatus):Canvas = {
    actualEyes = eyesStatus match {
      case EyesActive(r)=> activatedEyes
      case EyesDisabled => eyes
    }
    drawRepresentation
  }

  /**
    * Return a [[Canvas]] with the animal representation after setting the digestive system status
    * @param digestiveSystemStatus
    *                              The [[DigestiveSystemStatus]] to set
    * @return
    *         The [[Canvas]] after setting the digestive system status
    */
  def setDigestiveSystemStatus(digestiveSystemStatus: DigestiveSystemStatus):Canvas = {
    actualDigestiveSystem = digestiveSystemStatus match {
      case Digesting=> digestiveSystemActivated
      case NotDigesting => digestiveSystem
    }
    drawRepresentation
  }

  /**
    * Return a [[Canvas]] with the animal representation after setting the reproductive system status
    *
    * @param reproductiveApparatusStatus
    *                                    The [[ReproductiveApparatusStatus]] to set
    * @return
    * *         The [[Canvas]] after setting the reproductive system status
    */
  def setReproductiveSystemStatus(reproductiveApparatusStatus: ReproductiveApparatusStatus):Canvas = {
    actualDigestiveSystem = reproductiveApparatusStatus match {
      case Reproducing=> reproductiveSystemActivated
      case NotReproducing =>digestiveSystem
    }
    drawRepresentation
  }

}

/**
  * The [[AnimalRepresentation]] of a Male
  */
case class MaleAnimalRepresentation() extends AnimalRepresentation{
  override val digestiveSystem: Image = BodyImages.manDigestiveSystem
  override val digestiveSystemActivated: Image = BodyImages.manDigestiveSystemActivated
  override val reproductiveSystemActivated: Image = BodyImages.manReproductiveSystemActivated
  actualDigestiveSystem = digestiveSystem
}
/**
  *The trait that extends [[AnimalRepresentation]] with the additional method to modify the visualization of a female
  * internal status
  */
sealed trait FemaleRepresentation extends AnimalRepresentation{
  private var embryoStatus:Option[EmbryoStatus.Value] = None
  def setEmbryoStatus(embryoS: EmbryoStatus.Value):Unit = embryoStatus = Some(embryoS)
  val normalDigestiveSystem:Image = BodyImages.womenNormalDigestiveSystem
  val activatedDigestiveSystem:Image = BodyImages.womenActivatedDigestiveSystem
  override val reproductiveSystemActivated: Image = BodyImages.womenReproductiveSystemActivated

  val littleFetus:Image =  BodyImages.littleFetus
  val littleFetusDigesting:Image =  BodyImages.littleFetusDigesting

  val mediumFetus:Image = BodyImages.mediumFetus
  val mediumFetusDigesting:Image= BodyImages.mediumFetusDigesting

  val bigFetus:Image = BodyImages.bigFetus
  val bigFetusDigesting:Image =  BodyImages.bigFetusDigesting

//  override def drawRepresentation: Canvas = {
//    super.drawRepresentation
//  }
  override def digestiveSystem:Image = embryoStatus match {
    case Some(EmbryoStatus.primal) =>littleFetus
    case Some(EmbryoStatus.mid) =>mediumFetus
    case Some(EmbryoStatus.advanced) =>  bigFetus
    case _ => normalDigestiveSystem
  }
  override def digestiveSystemActivated:Image = embryoStatus match {
    case Some(EmbryoStatus.primal) =>littleFetusDigesting
    case Some(EmbryoStatus.mid) =>mediumFetusDigesting
    case Some(EmbryoStatus.advanced) =>  bigFetusDigesting
    case _ => activatedDigestiveSystem
  }

  override def setReproductiveSystemStatus(reproductiveApparatusStatus: ReproductiveApparatusStatus): Canvas
  = embryoStatus match {
    case Some(_) if reproductiveApparatusStatus == Reproducing => throw new IllegalStateException()
    case _ => super.setReproductiveSystemStatus(reproductiveApparatusStatus)
  }
}

case class FemaleAnimalRepresentation() extends FemaleRepresentation{
  actualDigestiveSystem = normalDigestiveSystem
}


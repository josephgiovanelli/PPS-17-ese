package it.unibo.pps.ese.view.sections.bodyviewer

import it.unibo.pps.ese.model.components.animals.trackers.EmbryoStatus

import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
import scalafx.scene.paint.Color
sealed trait AnimalRepresentation{
  /**
    * Method to obtain the current animal representation drawn in a Canvas
    * @return
    *         A [[Canvas]] with the current animal representation
    */
  def drawRepresentation:Canvas
  /** Return a [[Canvas]] with the animal representation after setting the brain status
    *
    * @param brainStatus The [[BrainStatus]] to set
    * @return The [[Canvas]] after setting the brain status
    */
  def setBrainStatus(brainStatus: BrainStatus):AnimalRepresentation
  /**
    * Return a [[Canvas]] with the animal representation after setting the eyes status
    * @param eyesStatus
    *                   The [[EyesStatus]] to set
    * @return
    *         The [[Canvas]] after setting the eyes status
    */
  def setEyesStatus(eyesStatus: EyesStatus):AnimalRepresentation

  /**
    * Return a [[Canvas]] with the animal representation after setting the digestive system status
    * @param digestiveSystemStatus
    *                              The [[DigestiveSystemStatus]] to set
    * @return
    *         The [[Canvas]] after setting the digestive system status
    */
  def setDigestiveSystemStatus(digestiveSystemStatus: DigestiveSystemStatus):AnimalRepresentation

  /**
    * Return a [[Canvas]] with the animal representation after setting the reproductive system status
    *
    * @param reproductiveApparatusStatus
    *                                    The [[ReproductiveApparatusStatus]] to set
    * @return
    * *         The [[Canvas]] after setting the reproductive system status
    */
  def setReproductiveSystemStatus(reproductiveApparatusStatus: ReproductiveApparatusStatus):AnimalRepresentation
}
/**
  * Abstract implementation of the Visual Representation of the animal's internal state
  */
sealed abstract class AbstractAnimalRepresentation extends AnimalRepresentation {
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


  override def drawRepresentation:Canvas = {
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


  override def setBrainStatus(brainStatus: BrainStatus):AnimalRepresentation = {
    actualBrain = brainStatus match {
      case HippoCampusActive(r)=> activatedBrain
      case HippoCampusDisabled(_) => brain
    }
    this
  }

  override def setEyesStatus(eyesStatus: EyesStatus):AnimalRepresentation = {
    actualEyes = eyesStatus match {
      case EyesActive(r)=> activatedEyes
      case EyesDisabled(_) => eyes
    }
    this
  }


  override def setDigestiveSystemStatus(digestiveSystemStatus: DigestiveSystemStatus):AnimalRepresentation = {
    actualDigestiveSystem = digestiveSystemStatus match {
      case Digesting=> digestiveSystemActivated
      case NotDigesting => digestiveSystem
    }
    this
  }

  override def setReproductiveSystemStatus(reproductiveApparatusStatus: ReproductiveApparatusStatus):AnimalRepresentation = {
    actualDigestiveSystem = reproductiveApparatusStatus match {
      case Reproducing=> reproductiveSystemActivated
      case NotReproducing =>digestiveSystem
    }
    this
  }

}

/**
  * The [[AbstractAnimalRepresentation]] of a Male
  */
case class MaleAnimalRepresentation() extends AbstractAnimalRepresentation{
  override val digestiveSystem: Image = BodyImages.manDigestiveSystem
  override val digestiveSystemActivated: Image = BodyImages.manDigestiveSystemActivated
  override val reproductiveSystemActivated: Image = BodyImages.manReproductiveSystemActivated
  actualDigestiveSystem = digestiveSystem
}
/**
  *The trait that extends [[AbstractAnimalRepresentation]] with the additional method to modify the visualization of a female
  * internal status
  */
sealed trait FemaleRepresentation extends AbstractAnimalRepresentation{
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

  override def setReproductiveSystemStatus(reproductiveApparatusStatus: ReproductiveApparatusStatus): AnimalRepresentation
  = embryoStatus match {
    case Some(_) if reproductiveApparatusStatus == Reproducing => throw new IllegalStateException()
    case _ => super.setReproductiveSystemStatus(reproductiveApparatusStatus)
  }
}

case class FemaleAnimalRepresentation() extends FemaleRepresentation{
  actualDigestiveSystem = normalDigestiveSystem
}


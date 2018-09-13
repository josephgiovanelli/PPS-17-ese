package it.unibo.pps.ese.view.bodyViewer

import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
import scalafx.scene.paint.Color

abstract class AnimalRepresentation{
  val brain: Image = BodyImages.brain
  val activatedBrain: Image = BodyImages.activatedBrain
  val eyes: Image = BodyImages.eyes
  val activatedEyes: Image = BodyImages.activatedEyes
  def digestiveSystem:Image
  def digestiveSystemActivated:Image
  val reproductiveSystemActivated:Image
  var actualBrain:Image = brain
  var actualEyes:Image = eyes
  var actualDigestiveSystem:Image=digestiveSystem

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
    gc.drawImage(actualEyes,55+pad,68.5)
    canvas
  }

  def setBrainStatus(brainStatus: BrainStatus):Canvas = {
    actualBrain = brainStatus match {
      case HippoCampusActive(r)=> activatedBrain
      case HippoCampusDisabled => brain
    }
    drawRepresentation
  }
  def setEyesStatus(eyesStatus: EyesStatus):Canvas = {
    actualEyes = eyesStatus match {
      case EyesActive(r)=> activatedEyes
      case EyesDisabled => eyes
    }
    drawRepresentation
  }
  def setDigestiveSystemStatus(digestiveSystemStatus: DigestiveSystemStatus):Canvas = {
    actualDigestiveSystem = digestiveSystemStatus match {
      case Digesting=> digestiveSystemActivated
      case NotDigesting => digestiveSystem
    }
    drawRepresentation
  }

  def setReproductiveSystemStatus(reproductiveApparatusStatus: ReproductiveApparatusStatus):Canvas = {
    actualDigestiveSystem = reproductiveApparatusStatus match {
      case Reproducing=> reproductiveSystemActivated
      case NotReproducing =>digestiveSystem
    }
    drawRepresentation
  }

}
case class MaleAnimalRepresentation() extends AnimalRepresentation{
  override val digestiveSystem: Image = BodyImages.manDigestiveSystem
  override val digestiveSystemActivated: Image = BodyImages.manDigestiveSystemActivated
  override val reproductiveSystemActivated: Image = BodyImages.manReproductiveSystemActivated
  actualDigestiveSystem = digestiveSystem

}

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

  override def drawRepresentation: Canvas = {
    actualDigestiveSystem = this.digestiveSystem
    super.drawRepresentation
  }
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

case class FemaleAnimalRepresentation() extends AnimalRepresentation with FemaleRepresentation{
  actualDigestiveSystem = normalDigestiveSystem
}


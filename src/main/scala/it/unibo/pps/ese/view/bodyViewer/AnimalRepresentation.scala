package it.unibo.pps.ese.view.bodyViewer

import it.unibo.pps.ese.entitybehaviors.EmbryoStatus
import it.unibo.pps.ese.view._
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
import scalafx.scene.paint.Color

abstract class AnimalRepresentation{
  val brain: Image = new Image("it.unibo.pps.ese.view/Common/brain.png")
  val activatedBrain: Image = new Image("it.unibo.pps.ese.view/Common/brainIppo.png")
  val eyes: Image = new Image("it.unibo.pps.ese.view/Common/eyesNormal.png")
  val activatedEyes: Image = new Image("it.unibo.pps.ese.view/Common/eyesBig2.png")
  def digestiveSystem:Image
  def digestiveSystemActivated:Image
  val reproductiveSystemActivated:Image
  var actualBrain:Image = brain
  var actualEyes:Image = eyes
  var actualDigestiveSystem:Image=digestiveSystem

  def drawRepresentation:Canvas = {
    val canvas = new Canvas(600,900)
    val gc = canvas.graphicsContext2D
    gc.fill = Color.color(0.2, 0.2, 0.2, 1.0)
    gc.fillRect(0,0,600,800)
    gc.fill = Color.White
    val pad:Double = 150.0
    gc.fillOval(0,0,550,800)
    gc.drawImage(actualDigestiveSystem,4+pad,200)
    gc.drawImage(actualBrain,22+pad,10)
    gc.drawImage(actualEyes,55+pad,88.5)
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
  override val digestiveSystem: Image = new Image("it.unibo.pps.ese.view/Man/digManN.png")
  override val digestiveSystemActivated: Image =new Image("it.unibo.pps.ese.view/Man/digManDig.png")
  override val reproductiveSystemActivated: Image = new Image("it.unibo.pps.ese.view/Man/digMSex.png")
  actualDigestiveSystem = digestiveSystem

}

sealed trait FemaleRepresentation extends AnimalRepresentation{
  private var embryoStatus:Option[EmbryoStatus.Value] = None
  def setEmbryoStatus(embryoS: EmbryoStatus.Value):Unit = embryoStatus = Some(embryoS)
  val normalDigestiveSystem:Image = new Image("it.unibo.pps.ese.view/Women/dig.png")
  val activatedDigestiveSystem:Image = new Image("it.unibo.pps.ese.view/Women/digDig.png")
  override val reproductiveSystemActivated: Image = new Image("it.unibo.pps.ese.view/women/digSex.png")

  val littleFetus:Image =  new Image("it.unibo.pps.ese.view/Women/pregnant/preg1.png")
  val littleFetusDigesting:Image =  new Image("it.unibo.pps.ese.view/Women/pregnant/preg1Dig.png")

  val mediumFetus:Image = new Image("it.unibo.pps.ese.view/Women/pregnant/preg4.png")
  val mediumFetusDigesting:Image= new Image("it.unibo.pps.ese.view/Women/pregnant/preg4Dig.png")

  val bigFetus:Image = new Image("it.unibo.pps.ese.view/Women/pregnant/preg8.png")
  val bigFetusDigesting:Image =  new Image("it.unibo.pps.ese.view/Women/pregnant/preg8Dig.png")

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


package it.unibo.pps.ese.view.sections.genome

 import it.unibo.pps.ese.model.genetics.dnaexpression.GeneStats
 import scalafx.Includes._
 import scalafx.scene.input.MouseEvent
 import scalafx.scene.paint.PhongMaterial
 import scalafx.scene.shape.{Cylinder, Shape3D, Sphere}
 import scalafx.scene.transform.{Rotate, Scale, Translate}

/**
  * A pair of spheres to which  assign information on genes
  */
sealed trait ModifiableSphereCouple{
  /**
    * To assign information about gene couple
    *
    * @param geneInformationCoupled
    *   The information to assign
    * @param lS
    *   The left [[GeneDetailsSubScene]] used to show the information
    * @param rS
    *   The right [[GeneDetailsSubScene]] used to show the information
    */
  def setGeneStats(geneInformationCoupled: GeneInformationCoupled, lS:GeneDetailsSubScene, rS:GeneDetailsSubScene):Unit

  /**
    * To clear the information assigned to the sphere couple
    */
  def clearGeneStats():Unit
}

/**
  * A [[ModifiableSphereCouple]] that extend [[Xform]], it consist in a group of two [[Sphere]] with two [[Cylinder]]
  *
  * @param s1
  *           Left [[Sphere]]
  * @param s2
  *           Right [[Sphere]]
  * @param c1
  *           Left [[Cylinder]]
  * @param c2
  *           Right [[Cylinder]]
  */
class GeneCoupleXForm(val s1:Sphere,val s2:Sphere,val c1:Cylinder,val c2:Cylinder) extends Xform() with ModifiableSphereCouple {
  override def setGeneStats(geneInformationCoupled: GeneInformationCoupled, lS:GeneDetailsSubScene, rS:GeneDetailsSubScene): Unit = {
    setMaterialsOfShapes(Materials.blueMaterial,s1,c2)
    setMaterialsOfShapes(Materials.greyMaterial,s2,c1)
    val clickListener: MouseEvent => Unit = (me: MouseEvent) => {
      val gCouple:GeneInformationCoupled = geneInformationCoupled
      val geneStats1:GeneStats = gCouple.gene1.geneStats
      val geneStats2:GeneStats = gCouple.gene2.geneStats
      val cType1 = geneInformationCoupled.gene1.chromosomeType
      val cType2 = geneInformationCoupled.gene2.chromosomeType
      val cName1:String = if (cType1==cType2) cType1.toString+" 1" else cType1.toString
      val cName2:String = if (cType1==cType2) cType2.toString+" 2" else cType2.toString

      lS.visualizeGeneStats(
        cName = cName1,
        geneStats = geneStats1
      )

      rS.visualizeGeneStats(
        cName = cName2,
        geneStats = geneStats2
      )
    }
    setListeners(s1,s2)(clickListener)
  }

  override def clearGeneStats(): Unit = {
    setMaterialsOfShapes(Materials.whiteMaterial,s1,s2)
    setMaterialsOfShapes(Materials.greyMaterial,c1,c2)
    val listener:MouseEvent => Unit = (me:MouseEvent)=> {}
    setListeners(s1,s2)(listener)
  }

  private def setPropertyOfShapes(shapes:Shape3D*)(f:Shape3D=>Unit):Unit ={
    shapes.foreach(f(_))
  }
  private def setMaterialsOfShapes(material:PhongMaterial,shapes:Shape3D*):Unit={
    setPropertyOfShapes(shapes:_*)(s =>s.material = material)
  }
  private def setListeners(shapes:Shape3D*)(listener:MouseEvent => Unit):Unit = {
    setPropertyOfShapes(shapes:_*)(s=>{
      s.onMouseClicked = listener
      s.onMouseEntered = listener
    })
  }
}

object Xform {
  object RotateOrder {
    final val XYZ = RotateOrder("XYZ")
    final val XZY = RotateOrder("XZY")
    final val YXZ = RotateOrder("YXZ")
    final val YZX = RotateOrder("YZX")
    final val ZXY = RotateOrder("ZXY")
    final val ZYX = RotateOrder("ZYX")
  }

  sealed case class RotateOrder(name: String)

}

/**
  * Custom [[javafx.scene.Group]] credits to
  * * ScalaFX implementation of `MoleculeSampleApp` from tutorial
  * * [[http://docs.oracle.com/javafx/8/3d_graphics/jfxpub-3d_graphics.htm Getting Started with JavaFX 3D Graphics]]
  * * by Cindy Castillo and John Yoon.
  * *
  * * @author Jarek Sacha
  */
class Xform extends javafx.scene.Group {
  private val _t = new Translate()
  private val p = new Translate()
  private val ip = new Translate()
  private val _rx = new Rotate {
    axis = Rotate.XAxis
  }
  private val _ry = new Rotate {
    axis = Rotate.YAxis
  }
  private val _rz = new Rotate {
    axis = Rotate.ZAxis
  }
  private val s = new Scale

  transforms ++= Seq(t, _rz, _ry, _rx, s)

  def this(rotateOrder: Xform.RotateOrder) {
    this()
    import Xform.RotateOrder._
    rotateOrder match {
      case XYZ => transforms ++= Seq(t, p, _rz, _ry, _rx, s, ip)
      case XZY => transforms ++= Seq(t, p, _ry, _rz, _rx, s, ip)
      case YXZ => transforms ++= Seq(t, p, _rz, _rx, _ry, s, ip)
      case YZX => transforms ++= Seq(t, p, _rx, _rz, _ry, s, ip)
      case ZXY => transforms ++= Seq(t, p, _ry, _rx, _rz, s, ip)
      case ZYX => transforms ++= Seq(t, p, _rx, _ry, _rz, s, ip)
      case _=>
    }
  }

  def children = getChildren

  def transforms = getTransforms

  def setTranslate(x: Double, y: Double, z: Double) {
    t.x = x
    t.y = y
    t.z = z
  }

  def setTranslate(x: Double, y: Double) {
    t.x = x
    t.y = y
  }

  def rx : Rotate = _rx
  def ry : Rotate = _ry
  def rz : Rotate = _rz
  def t : Translate = _t

  def setRotate(x: Double, y: Double, z: Double) {
    _rx.angle = x
    _ry.angle = y
    _rz.angle = z
  }

  def setRotateX(x: Double) {
    _rx.angle = x
  }

  def rotateY = _ry.angle
  def rotateY_=(y: Double) {
    _ry.angle = y
  }

  def rotateZ:Double = _rz.angle()
  def rotateZ_=(z: Double) {
    _rz.angle = z
  }

  def setRx(x: Double) {
    _rx.angle = x
  }

  def setRy(y: Double) {
    _ry.angle = y
  }

  def setRz(z: Double) {
    _rz.angle = z
  }

  def setScale(scaleFactor: Double) {
    s.x = scaleFactor
    s.y = scaleFactor
    s.z = scaleFactor
  }

  def setScale(x: Double, y: Double, z: Double) {
    s.x=x
    s.y=y
    s.z = z
  }

  def setSx(x: Double) {
    s.x = x
  }

  def setSy(y: Double) {
    s.y = y
  }

  def setSz(z: Double) {
    s.z = z
  }

  def setPivot(x: Double, y: Double, z: Double) {
    p.x = x
    p.y = y
    p.z = z
    ip.x = -x
    ip.y = -y
    ip.z = -z
  }

  def reset() {
    t.x = 0.0
    t.y = 0.0
    t.z = 0.0
    _rx.angle = 0.0
    _ry.angle = 0.0
    _rz.angle = 0.0
    s.x = 1.0
    s.y = 1.0
    s.z = 1.0
    p.x = 0.0
    p.y = 0.0
    p.z = 0.0
    ip.x = 0.0
    ip.y = 0.0
    ip.z = 0.0
  }

  def resetTSP() {
    t.x = 0.0
    t.y = 0.0
    t.z = 0.0
    s.x = 1.0
    s.y = 1.0
    s.z = 1.0
    p.x = 0.0
    p.y = 0.0
    p.x = 0.0
    ip.x = 0.0
    ip.y = 0.0
    ip.z = 0.0
  }
}


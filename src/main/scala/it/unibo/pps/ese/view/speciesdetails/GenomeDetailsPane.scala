package it.unibo.pps.ese.view.speciesdetails

import javafx.scene.text.{Font, Text}
import scalafx.Includes._
import scalafx.scene._
import scalafx.geometry.Insets
import scalafx.scene.control.Slider
import scalafx.scene.paint.{Color, Paint, PhongMaterial}
import scalafx.scene.transform.Rotate
import scalafx.scene.shape._
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.text.TextAlignment
import TextUtilities._
sealed trait GenomeDetailsPane extends Pane{
  def setGenomeStats(genomeStats:List[ChromosomeWithGeneCouple]):Unit
}
object GenomeDetailsPane {
  def apply(genomeStats: Option[List[ChromosomeWithGeneCouple]]):GenomeDetailsPane= new SpeciesDetailsPane(genomeStats)
  private[this] class SpeciesDetailsPane(genomeStats:Option[List[ChromosomeWithGeneCouple]]) extends GenomeDetailsPane {
    prefWidth = 1000
    prefHeight = 800

    val ballSize:Double = 15.0
    val cylinderRadius:Double = 3.0
    val cylinderHeight:Double = 60.0
    background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))

    val root = new Group()
    children += root
    val tbox = new VBox()
    tbox.spacing = 5
    val text = new Text("Entity Genome")
    text.setWrappingWidth(1200)
    text.textAlignment = TextAlignment.Center
    val textStyle:String = "-fx-font-size:40;"
    text.setStyle(textStyle)
    text.setFill(Color.White)
    text.setFont(Font.font("Calibri"))
    tbox.children.addAll(text)
//    tbox.setLayoutX(450)
    tbox.setLayoutY(10)
    val textDetails = "Select a sphere to get the information about the gene couple".toText
    textDetails.textAlignment = TextAlignment.Center
    textDetails.wrappingWidth = 1200
    tbox.children += textDetails
    val hbox = new HBox()
    hbox.setLayoutX(20)
    hbox.setLayoutY(150)
    var molecules:List[GeneCoupleXForm] = List()
    for (i <- 0 until 20) molecules = MoleculeCreator.createNextMolecule::molecules

    val moleculeGroup = new Group()
    moleculeGroup.children ++= molecules

    val cylinder1 = new Group()
    cylinder1.children += moleculeGroup

    val lS = new GeneDetailsSubScene(400,800,Left)
    hbox.children += lS
    val msaa = createSubScene(cylinder1,
      Color.Transparent,
      new PerspectiveCamera(), true)

    hbox.getChildren.add(msaa)


    val rS = new GeneDetailsSubScene(400,800,Right)
    hbox.children += rS
    val slider = new Slider(0, 360, 0)
    slider.setBlockIncrement(1)
    slider.setTranslateX(500)
    slider.setTranslateY(625)
    cylinder1.rotateProperty().bind(slider.valueProperty())
    root.getChildren.addAll(tbox,hbox, slider)
    if(genomeStats.nonEmpty) setGenomeStats(genomeStats.get)

    override def setGenomeStats(genomeStats: List[ChromosomeWithGeneCouple]): Unit = {

      for(i<-genomeStats.indices) molecules(i).setGeneStats(genomeStats(i),lS,rS)

    }

    def createSubScene(node:Node,
                       fillPaint:Paint,  camera:Camera, msaa:Boolean):SubScene={
      val father = new Group()

      node.setRotationAxis(Rotate.YAxis)
      node.setTranslateX(150)
      node.setTranslateY(250)
      father.getChildren.addAll(node)

      val subScene = new SubScene(father, 400, 600, true,SceneAntialiasing.Balanced)
      subScene.setFill(fillPaint)
      subScene.setCamera(camera)

      subScene
    }
    private[this] object MoleculeCreator {
      val initialPos: Double = 180
      var actualPos: Double = initialPos
      val initialRotation1: Double = -135
      val initialRotation2: Double = 45
      var actualRotation1: Double = initialRotation1
      var actualRotation2: Double = initialRotation2
      import CreateDna._
      def createNextMolecule(): GeneCoupleXForm = {
        val molecule = createMoleculeFormComplete(actualPos, actualPos, actualRotation1, actualRotation2)
        actualPos -= 20
        actualRotation1 += 15
        actualRotation2 += 15
        molecule
      }
    }
    private[this] object CreateDna{

      def createHydrogenSpereCouple(tY:Double):(Sphere,Sphere) = {
        val size:Double = ballSize
        import Materials._
        val hydrogen1Sphere = new Sphere(size) {
          material = whiteMaterial
          translateY = tY
        }

        val hydrogen2Sphere = new Sphere(size) {
          material = whiteMaterial
          translateY = tY
        }
        val clickListener:MouseEvent=>Unit = (me:MouseEvent) =>{
          lS.emptyGeneStats()
          rS.emptyGeneStats()
        }
        hydrogen1Sphere.onMouseClicked = clickListener
        hydrogen2Sphere.onMouseClicked = clickListener

        hydrogen1Sphere.onMouseEntered = clickListener
        hydrogen2Sphere.onMouseEntered = clickListener
        (hydrogen1Sphere,hydrogen2Sphere)
      }

      def createCylinderCouple(tX:Double,tY:Double,r:Double):(Cylinder,Cylinder) = {
        val cSize:(Double,Double) = (cylinderRadius,cylinderHeight)
        val bc1 = new Cylinder(cSize._1, cSize._2) {
          material = Materials.greyMaterial
          translateX = tX
          translateY = tY
          rotationAxis = Rotate.ZAxis
          rotate = r
        }

        val bc2 = new Cylinder(cSize._1, cSize._2) {
          material = Materials.blueMaterial
          translateX = tX
          translateY = tY
          rotationAxis = Rotate.ZAxis
          rotate = r
        }
        (bc1,bc2)
      }
      def createMoleculeForm(r1:Double,r2:Double,s1:Sphere,s2:Sphere,c1:Cylinder,c2:Cylinder):GeneCoupleXForm = {
        new GeneCoupleXForm(s1,s2) {
          children ++= Seq(
            new Xform {
              rotateY = r1
              children ++= Seq(
                new Xform {
                  children += s1
                  t.x = cylinderHeight
                },
                c1
              )
            },
            // Hydrogen 2
            new Xform {
              rotateY = r2
              children ++= Seq(
                new Xform {
                  children += s2
                  t.x = cylinderHeight
                },
                c2
              )
            }
          )
        }
      }

      def createMoleculeFormComplete(cylinderY:Double,sphereY:Double,r1:Double,r2:Double):GeneCoupleXForm = {
        val b1 = createCylinderCouple(cylinderHeight/2,cylinderY,90.0)
        val c1 = createHydrogenSpereCouple(sphereY)
        createMoleculeForm(r1,r2,c1._1,c1._2,b1._1,b1._2)
      }
    }

  }

}

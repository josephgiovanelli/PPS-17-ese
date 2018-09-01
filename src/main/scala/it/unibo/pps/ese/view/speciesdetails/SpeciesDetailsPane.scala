package it.unibo.pps.ese.view.speciesdetails


import it.unibo.pps.ese.genetics.dnaexpression.GeneStats
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

class SpeciesDetailsPane(genomeStats:List[ChromosomeWithGeneCouple]) extends Pane {
  prefWidth = 1000
  prefHeight = 800

  val ballSize:Double = 15.0
  val cylinderRadius:Double = 3.0
  val cylinderHeight:Double = 60.0
  val root = new Group()

  children += root
  background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))

  val hbox = new HBox()
  hbox.setLayoutX(20)
  hbox.setLayoutY(150)
  var molecules:List[Xform] = genomeStats.map(g=>MoleculeCreator.createNextMolecule(Some(g)))
  for (i <- molecules.size until 20) molecules = MoleculeCreator.createNextMolecule(None)::molecules

  private val moleculeGroup = new Group()
  moleculeGroup.children ++= molecules

  val cylinder1 = new Group()
  cylinder1.children += moleculeGroup

  val lS = new GeneDetailsSubScene(300,800,Left)
  hbox.children += lS
  val msaa = createSubScene("", cylinder1,
    Color.Transparent,
    new PerspectiveCamera(), true)

  hbox.getChildren.add(msaa)

  val rS = new GeneDetailsSubScene(300,800,Right)
  hbox.children += rS
  val slider = new Slider(0, 360, 0)
  slider.setBlockIncrement(1)
  slider.setTranslateX(400)
  slider.setTranslateY(625)
  cylinder1.rotateProperty().bind(slider.valueProperty())
  root.getChildren.addAll(hbox, slider)
  def setTitle (str:String,text: Text):Parent= {
    val vbox = new VBox()
    text.setText(str)
    text.setFont(Font.font("Calibri", 24))
    text.setFill(Color.web("67809F"))
    vbox.getChildren.add(text)
    vbox
  }

  def createSubScene( title:String,  node:Node,
                      fillPaint:Paint,  camera:Camera, msaa:Boolean):SubScene={
    val father = new Group()

    node.setRotationAxis(Rotate.YAxis)
    node.setTranslateX(150)
    node.setTranslateY(250)
    father.getChildren.addAll(setTitle(title,new Text()), node)

    val subScene = new SubScene(father, 400, 600, true,SceneAntialiasing.Balanced)
    subScene.setFill(fillPaint)
    subScene.setCamera(camera)

    subScene
  }
  object MoleculeCreator {
    val initialPos: Double = 180
    var actualPos: Double = initialPos
    val initialRotation1: Double = -135
    val initialRotation2: Double = 45
    var actualRotation1: Double = initialRotation1
    var actualRotation2: Double = initialRotation2
    import CreateDna._
    def createNextMolecule(chromosomeWithGeneCouple: Option[ChromosomeWithGeneCouple]): Xform = {
      val molecule = createMoleculeFormComplete(actualPos, actualPos, actualRotation1, actualRotation2, chromosomeWithGeneCouple)
      actualPos -= 20
      actualRotation1 += 15
      actualRotation2 += 15
      molecule
    }
  }
  object CreateDna{
    private val blueMaterial:PhongMaterial = new PhongMaterial {
      diffuseColor = Color.web("1abc9c")
      specularColor = Color.web("16a085")
    }
    private val greyMaterial:PhongMaterial = new PhongMaterial {
      diffuseColor = Color.web("95a5a6")
      specularColor = Color.web("7f8c8d")
    }
    private val whiteMaterial:PhongMaterial = new PhongMaterial{
      diffuseColor = Color.web("ecf0f1")
      specularColor = Color.web("bdc3c7")
    }
    def createHydrogenSpereCouple(tY:Double,chromosomeWithGeneCouple: Option[ChromosomeWithGeneCouple]):(Sphere,Sphere) = {
      val size:Double = ballSize

      val hydrogen1Sphere = new Sphere(size) {
        material = if (chromosomeWithGeneCouple.nonEmpty) blueMaterial else whiteMaterial
        translateY = tY
      }

      val hydrogen2Sphere = new Sphere(size) {
        material = if (chromosomeWithGeneCouple.nonEmpty) greyMaterial else whiteMaterial
        translateY = tY
      }
      val clickListener:MouseEvent=>Unit = (me:MouseEvent) =>{
        if(chromosomeWithGeneCouple.nonEmpty){
          val cType = chromosomeWithGeneCouple.get.chromosomeType
          val gCouple:GeneCouple = chromosomeWithGeneCouple.get.geneCouple
          val geneStats1:GeneStats = gCouple.gene1
          val geneStats2:GeneStats = gCouple.gene2
          lS.visualizeGeneStats(
            cName = cType.toString+" 1",
            geneStats = geneStats1
          )

          rS.visualizeGeneStats(
            cName = cType.toString+" 2",
            geneStats = geneStats2
          )
        }else{
          lS.emptyGeneStats()
          rS.emptyGeneStats()
        }
      }
      hydrogen1Sphere.onMouseClicked = clickListener

      hydrogen2Sphere.onMouseClicked = clickListener
      (hydrogen1Sphere,hydrogen2Sphere)
    }

    def createCylinderCouple(tX:Double,tY:Double,r:Double):(Cylinder,Cylinder) = {
      val cSize:(Double,Double) = (cylinderRadius,cylinderHeight)
      val bc1 = new Cylinder(cSize._1, cSize._2) {
        material = greyMaterial
        translateX = tX
        translateY = tY
        rotationAxis = Rotate.ZAxis
        rotate = r
      }

      val bc2 = new Cylinder(cSize._1, cSize._2) {
        material = blueMaterial
        translateX = tX
        translateY = tY
        rotationAxis = Rotate.ZAxis
        rotate = r
      }
      (bc1,bc2)
    }
    def createMoleculeForm(r1:Double,r2:Double,s1:Sphere,s2:Sphere,c1:Cylinder,c2:Cylinder):Xform = {
      new Xform {
        children ++= Seq(
          // Oxygen
          //        oxygenSphere,
          // Hydrogen 1
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

    def createMoleculeFormComplete(cylinderY:Double,sphereY:Double,r1:Double,r2:Double,chromosomeWithGeneCouple: Option[ChromosomeWithGeneCouple]):Xform = {
      val b1 = createCylinderCouple(cylinderHeight/2,cylinderY,90.0)
      val c1 = createHydrogenSpereCouple(sphereY,chromosomeWithGeneCouple)
      createMoleculeForm(r1,r2,c1._1,c1._2,b1._1,b1._2)
    }
  }

}

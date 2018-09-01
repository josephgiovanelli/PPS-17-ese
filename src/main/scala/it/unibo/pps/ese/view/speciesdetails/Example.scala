package it.unibo.pps.ese.view.speciesdetails

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genetics.dna.{AnimalGenome, BasicGene, ChromosomeType, GeneWithAllelicForms, MGene}
import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.dnaexpression.GeneStats
import it.unibo.pps.ese.genetics.entities.AnimalInfo
import javafx.scene.text.{Font, Text}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene._
import scalafx.animation.Timeline
import scalafx.geometry.Point3D
import scalafx.scene.control.Slider
import scalafx.scene.paint.{Color, Paint, PhongMaterial}
import scalafx.scene.transform.Rotate
import scalafx.scene.shape._
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.{HBox, Pane, VBox}

object Example extends JFXApp { app =>
  val data = new YamlLoader().loadSimulation("it/unibo/pps/ese/controller/loader/Simulation.yml")
  val geneticsSimulator:GeneticsSimulator = GeneticsSimulator
  geneticsSimulator.beginSimulation(data)
  val animalInfo:AnimalInfo = geneticsSimulator.newAnimal("Gatto")
  val genome:AnimalGenome = animalInfo.genome
  val coupledGene:Map[ChromosomeType,List[(MGene,MGene)]] = genome.coupledGene
  val iteratorCommon = coupledGene(ChromosomeType.COMMON).iterator
  val iteratorStructural = coupledGene(ChromosomeType.STRUCTURAL_ANIMAL).iterator
  val iteratorLifeCycle = coupledGene(ChromosomeType.LIFE_CYCLE).iterator
  val iteratorFeeding = coupledGene(ChromosomeType.FEEDING).iterator
  val ballSize:Double = 15.0
  val cylinderRadius:Double = 3.0
  val cylinderHeight:Double = 60.0
    stage = new JFXApp.PrimaryStage()
    val root = new Group();
    val scene = new Scene(root, 1000, 800);
    scene.setFill(Color.color(0.2, 0.2, 0.2, 1.0))

    val hbox = new HBox()
    hbox.setLayoutX(20);
    hbox.setLayoutY(200);
  import CreateDna._
  val molecules:Seq[Xform] =  createMoleculeFormComplete(0.0,0.0,0.0,180,None,None) ::
  createMoleculeFormComplete(20,20,-15,165,Some(ChromosomeType.COMMON),Some(iteratorCommon))::
  createMoleculeFormComplete(-20,-20,15,195,Some(ChromosomeType.COMMON),Some(iteratorCommon))::
  createMoleculeFormComplete(40,40,-30,150,Some(ChromosomeType.STRUCTURAL_ANIMAL),Some(iteratorStructural))::
  createMoleculeFormComplete(60,60,-45,135,Some(ChromosomeType.STRUCTURAL_ANIMAL),Some(iteratorStructural))::
  createMoleculeFormComplete(80,80,-60,120,Some(ChromosomeType.LIFE_CYCLE),Some(iteratorLifeCycle))::
  createMoleculeFormComplete(100,100,-75,105,Some(ChromosomeType.LIFE_CYCLE),Some(iteratorLifeCycle))::
  createMoleculeFormComplete(-40,-40,30,210,Some(ChromosomeType.LIFE_CYCLE),Some(iteratorLifeCycle))::
  createMoleculeFormComplete(-60,-60,45,225,Some(ChromosomeType.LIFE_CYCLE),Some(iteratorLifeCycle))::
  createMoleculeFormComplete(-80,-80,60,240,Some(ChromosomeType.LIFE_CYCLE),Some(iteratorLifeCycle))::
  createMoleculeFormComplete(-100,-100,75,255,Some(ChromosomeType.FEEDING),Some(iteratorFeeding))::
  createMoleculeFormComplete(-120,-120,90,270,None,None)::
  createMoleculeFormComplete(-140,-140,105,285,None,None)::
  createMoleculeFormComplete(-160,-160,120,300,None,None)::
  createMoleculeFormComplete(120,120,-90,90,None,None)::
  createMoleculeFormComplete(140,140,-105,75,None,None)::
  createMoleculeFormComplete(160,160,-120,60,None,None)::
  createMoleculeFormComplete(180,180,-135,45,None,None)::
  List()

  private val moleculeGroup = new Group()
  moleculeGroup.children ++= molecules

  val phongMaterial = new PhongMaterial(Color.color(1.0, 0.7, 0.8));
  val cylinder1 = new Group()
  cylinder1.children += moleculeGroup

  val lS = new GeneDetailsSubScene(300,600,Left)
  hbox.children += lS
  val msaa = createSubScene("", cylinder1,
    Color.Transparent,
    new PerspectiveCamera(), true);
  hbox.getChildren().add(msaa);

  val rS = new GeneDetailsSubScene(300,600,Right)
  hbox.children += rS
  val slider = new Slider(0, 360, 0);
    slider.setBlockIncrement(1);
    slider.setTranslateX(400);
    slider.setTranslateY(625);
    cylinder1.rotateProperty().bind(slider.valueProperty());
    root.getChildren().addAll(hbox, slider);

    stage.setScene(scene);
    stage.show();

  def setTitle (str:String,text: Text):Parent= {
    val vbox = new VBox()
    text.setText(str)
    text.setFont(Font.font("Calibri", 24))
    text.setFill(Color.web("67809F"))
    vbox.getChildren().add(text)
    vbox
  }

  def createSubScene( title:String,  node:Node,
     fillPaint:Paint,  camera:Camera, msaa:Boolean):SubScene={
    val father = new Group();

    node.setRotationAxis(Rotate.YAxis);
    node.setTranslateX(150);
    node.setTranslateY(200);
    father.getChildren().addAll(setTitle(title,new Text()), node);

    val subScene = new SubScene(father, 400, 500, true,SceneAntialiasing.Balanced);
    subScene.setFill(fillPaint);
    subScene.setCamera(camera);

     subScene
  }
  object CreateDna{
    val blueMaterial = new PhongMaterial {
      diffuseColor = Color.web("1abc9c")
      specularColor = Color.web("16a085")
    }
    val greyMaterial = new PhongMaterial {
      diffuseColor = Color.web("95a5a6")
      specularColor = Color.web("7f8c8d")
    }
    val whiteMaterial = new PhongMaterial{
      diffuseColor = Color.web("ecf0f1")
      specularColor = Color.web("bdc3c7")
    }
    def createHydrogenSpereCouple(tY:Double,chromosomeType: Option[ChromosomeType],geneCouple:Option[(MGene,MGene)]):(Sphere,Sphere) = {
      val size:Double = ballSize

      val hydrogen1Sphere = new Sphere(size) {
        material = if (geneCouple.nonEmpty) blueMaterial else whiteMaterial
        translateY = tY
      }

      val hydrogen2Sphere = new Sphere(size) {
        material = if (geneCouple.nonEmpty) greyMaterial else whiteMaterial
        translateY = tY
      }
      val clickListener:MouseEvent=>Unit = (me:MouseEvent) =>{
        if(chromosomeType.nonEmpty && geneCouple.nonEmpty){
          val cType = chromosomeType.get
          val gCouple:(MGene,MGene) = geneCouple.get
          val geneStats1:GeneStats = geneticsSimulator.getGeneStats(gCouple._1,animalInfo)
          val geneStats2:GeneStats = geneticsSimulator.getGeneStats(gCouple._2,animalInfo)

          lS.visualizeGeneStats(
            cName = cType.toString+" 1",
            geneStats = geneStats1
          )

          rS.visualizeGeneStats(
            cName = cType.toString+" 2",
            geneStats = geneStats2
          )
        }
        println("Cliccato")
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
                t.x = Example.cylinderHeight
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
                t.x = Example.cylinderHeight
              },
              c2
            )
          }
        )
      }
    }

    def createMoleculeFormComplete(cylinderY:Double,sphereY:Double,r1:Double,r2:Double,chromosomeType: Option[ChromosomeType],iterator:Option[Iterator[(MGene,MGene)]]):Xform = {
      var geneCouple:Option[(MGene,MGene)] = None
      if(iterator.nonEmpty){
        if(iterator.get.hasNext){
          val element = iterator.get.next()
          geneCouple = Some(element)
        }
      }
      val b1 = createCylinderCouple(cylinderHeight/2,cylinderY,90.0)
      val c1 = createHydrogenSpereCouple(sphereY,chromosomeType,geneCouple)
      createMoleculeForm(r1,r2,c1._1,c1._2,b1._1,b1._2)
    }
  }

}
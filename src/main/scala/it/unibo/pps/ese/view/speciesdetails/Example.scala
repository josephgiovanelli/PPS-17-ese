package it.unibo.pps.ese.view.speciesdetails

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

  stage = new JFXApp.PrimaryStage()
    val root = new Group();
    val scene = new Scene(root, 1000, 800);
    scene.setFill(Color.color(0.2, 0.2, 0.2, 1.0));

    val hbox = new HBox()
    hbox.setLayoutX(50);
    hbox.setLayoutY(200);
  import CreateDna._
  val leftTextPane = new Text("")
  val rightTextPane = new Text("")
  import CreateDna._
  val moleculeXform = createMoleculeFormComplete(0.0,0.0,0.0,180)
  val moleculeXform2 = createMoleculeFormComplete(20,20,-15,165)
  val moleculeXform3 = createMoleculeFormComplete(-20,-20,15,195)
  val moleculeXform4 = createMoleculeFormComplete(40,40,-30,150)
  val moleculeXform5 = createMoleculeFormComplete(60,60,-45,135)
  val moleculeXform6 = createMoleculeFormComplete(80,80,-60,120)
  val moleculeXform7 = createMoleculeFormComplete(100,100,-75,105)
  val moleculeXform8 = createMoleculeFormComplete(-40,-40,30,210)
  val moleculeXform9 = createMoleculeFormComplete(-60,-60,45,225)
  val moleculeXform10 = createMoleculeFormComplete(-80,-80,60,240)
  val moleculeXform11 = createMoleculeFormComplete(-100,-100,75,255)
  val moleculeXform12 = createMoleculeFormComplete(-120,-120,90,270)
  val moleculeXform13 = createMoleculeFormComplete(-140,-140,105,285)
  val moleculeXform14 = createMoleculeFormComplete(-160,-160,120,300)
  val moleculeXform15 = createMoleculeFormComplete(120,120,-90,90)
  val moleculeXform16 = createMoleculeFormComplete(140,140,-105,75)
  val moleculeXform17 = createMoleculeFormComplete(160,160,-120,60)
  val moleculeXform18 = createMoleculeFormComplete(180,180,-135,45)

  private final val moleculeGroup = new Group()
  moleculeGroup.children += moleculeXform
  moleculeGroup.children += moleculeXform2
  moleculeGroup.children += moleculeXform3
  moleculeGroup.children += moleculeXform4
  moleculeGroup.children += moleculeXform5
  moleculeGroup.children += moleculeXform6
  moleculeGroup.children += moleculeXform7
  moleculeGroup.children += moleculeXform8
  moleculeGroup.children += moleculeXform9
  moleculeGroup.children += moleculeXform10
  moleculeGroup.children += moleculeXform11
  moleculeGroup.children += moleculeXform12
  moleculeGroup.children += moleculeXform13
  moleculeGroup.children += moleculeXform14
  moleculeGroup.children += moleculeXform15
  moleculeGroup.children += moleculeXform16
  moleculeGroup.children += moleculeXform17
  moleculeGroup.children += moleculeXform18


  val phongMaterial = new PhongMaterial(Color.color(1.0, 0.7, 0.8));
  val cylinder1 = new Group()
  cylinder1.children += moleculeGroup
  val cylinder2 = new Cylinder(100, 200);
  cylinder2.setMaterial(phongMaterial);

//  val cylinder1 = new Cylinder(100, 200);
//    cylinder1.setMaterial(phongMaterial);
//    val noMsaa = createSubScene("MSAA = false", cylinder2,
//      Color.Transparent,
//      new PerspectiveCamera(), false);
//    hbox.getChildren().add(noMsaa);
  val lS = createTextSubScene("Scena sinistra",Color.Transparent,leftTextPane)
  hbox.children += lS
  val msaa = createSubScene("", cylinder1,
    Color.Transparent,
    new PerspectiveCamera(), true);
  hbox.getChildren().add(msaa);

  val rS = createTextSubScene("Scena Destra",Color.Transparent,rightTextPane)
  hbox.children += rS
//  val msaa2 = createSubScene("MSAA = true", cylinder2,
//    Color.Transparent,
//    new PerspectiveCamera(), true);
//  hbox.getChildren().add(msaa2);

  val slider = new Slider(0, 360, 0);
    slider.setBlockIncrement(1);
    slider.setTranslateX(425);
    slider.setTranslateY(625);
    cylinder1.rotateProperty().bind(slider.valueProperty());
    cylinder2.rotateProperty().bind(slider.valueProperty());
    root.getChildren().addAll(hbox, slider);

    stage.setScene(scene);
    stage.show();

  def setTitle (str:String,text: Text):Parent= {
    val vbox = new VBox()
    text.setText(str)
    text.setFont(Font.font("Times New Roman", 24))
    text.setFill(Color.Wheat)
    vbox.getChildren().add(text)
    vbox
  }

  def createSubScene( title:String,  node:Node,
     fillPaint:Paint,  camera:Camera, msaa:Boolean):SubScene={
    val root = new Group();

    val light = new PointLight(Color.White);
    light.setTranslateX(50);
    light.setTranslateY(-300);
    light.setTranslateZ(-400);
    val light2 = new PointLight(Color.color(0.6, 0.3, 0.4));
    light2.setTranslateX(400);
    light2.setTranslateY(0);
    light2.setTranslateZ(-400);

    val ambientLight = new AmbientLight(Color.White);
    node.setRotationAxis(Rotate.YAxis);
    node.setTranslateX(125);
    node.setTranslateY(200);
    root.getChildren().addAll(setTitle(title,new Text()), ambientLight,
      light, light2, node);

    val subScene = new SubScene(root, 400, 500, true,SceneAntialiasing.Balanced);
    subScene.setFill(fillPaint);
    subScene.setCamera(camera);

     subScene
  }
  def createTextSubScene( title:String,fillPaint:Paint,text:Text):SubScene={
    val root = new Group();

    root.getChildren().addAll(setTitle(title,text));

    val subScene = new SubScene(root, 300, 400, true,SceneAntialiasing.Balanced);
    subScene.setFill(fillPaint);
    subScene
  }
  object CreateDna{
    val blueMaterial = new PhongMaterial {
      diffuseColor = Color.DarkBlue
      specularColor = Color.Blue
    }
    val greyMaterial = new PhongMaterial {
      diffuseColor = Color.DarkGrey
      specularColor = Color.Grey
    }
    def createHydrogenSpereCouple(tY:Double):(Sphere,Sphere) = {
      val size:Double = 15.0
      val hydrogen1Sphere = new Sphere(size) {
        material = blueMaterial
        translateY = tY
      }

      val hydrogen2Sphere = new Sphere(size) {
        material = greyMaterial
        translateY = tY
      }
      hydrogen1Sphere.onMouseClicked = (me:MouseEvent) =>{
        leftTextPane.setText("Sfera 1")
        rightTextPane.setText("Sfera 2")
        println("Cliccato")
      }
      hydrogen2Sphere.onMouseClicked = (me:MouseEvent) =>{
        leftTextPane.setText("Sfera 1")
        rightTextPane.setText("Sfera 2")
        println("Cliccato")
      }
      (hydrogen1Sphere,hydrogen2Sphere)
    }

    def createCylinderCouple(tX:Double,tY:Double,r:Double):(Cylinder,Cylinder) = {
      val cSize:(Double,Double) = (3,60)
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
    def createMoleculeForm(r1:Double,r2:Double,s1:Sphere,s2:Sphere,c1:Cylinder,c2:Cylinder,cu1:Box,cu2:Box):Xform = {
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
                t.x = 60
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
                t.x = 60
              },
              c2
            )
          },cu1,cu2
        )
      }
    }

    def createMoleculeFormComplete(cylinderY:Double,sphereY:Double,r1:Double,r2:Double):Xform = {
      val cube = new Box(100,70,1) {
        material = greyMaterial
        translateX = 100
        translateY = cylinderY
        rotationAxis = Rotate.ZAxis
        rotate = 90
      }
      val cube2 = new Box(50,50,1) {
        material = greyMaterial
        translateX = -100
        translateY = cylinderY
        rotationAxis = Rotate.ZAxis
        rotate = 90
      }
      cube.setVisible(false)
      cube2.setVisible(false)
      val b1 = createCylinderCouple(30.0,cylinderY,90.0)
      val c1 = createHydrogenSpereCouple(sphereY)
      createMoleculeForm(r1,r2,c1._1,c1._2,b1._1,b1._2,cube,cube2)
    }
  }

}
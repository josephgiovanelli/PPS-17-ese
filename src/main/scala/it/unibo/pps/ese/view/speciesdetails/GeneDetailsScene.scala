package it.unibo.pps.ese.view.speciesdetails
sealed trait Side
case object Left extends Side
case object Right extends Side

import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.dnaexpression.{AllelicGeneStats, BasicGeneStats, GeneStats}
import it.unibo.pps.ese.view.speciesdetails.Example.setTitle
import javafx.scene.text.{Font, Text}
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.scene.text.TextFlow
import scalafx.scene.{Group, Parent, SceneAntialiasing, SubScene}

import scala.collection.JavaConverters._
trait GeneDetails{
//  def setGeneDetails(
//                    chromosomeName:String,
//                    geneId:String,
//                    alleleId:String,
//                    affectedQualities:Seq[String],
//                    dominanceLevel:String,
//                    probability:String,
//                    active:String)
  def visualizeGeneStats(geneStats: GeneStats,cName:String)
  def emptyGeneStats():Unit
}

class GeneDetailsScene(width:Double,height:Double,side: Side) extends SubScene(width,height,true,SceneAntialiasing.Balanced) with GeneDetails {
  private val textLabel = new TextFlow()
  textLabel.children.add("No Gene Selected".toText)
  private[this]def buildDetailsBox(textLabel: TextFlow):Parent= {
    val cssLayout:String = side match {
      case Left =>
        "-fx-padding: 5 5 5 5;\n"+
        "-fx-border-color: white;\n" +
        "-fx-border-insets: 5;\n" +
        "-fx-border-width: 0 0 3 3;\n" +
        "-fx-border-style: solid;\n";
      case Right =>
        "-fx-padding: 5 5 5 5;\n"+
        "-fx-border-color: white;\n" +
        "-fx-border-insets: 5;\n" +
        "-fx-border-width: 3 3 0 0;\n" +
        "-fx-border-style: solid;\n";
    }
    val vbox = new VBox()
    vbox.setStyle(cssLayout);
    textLabel.setMaxWidth(width*0.85)
    vbox.getChildren().add(textLabel)

    vbox
  }

  val group = new Group()
  group.children.addAll(buildDetailsBox(textLabel))
  root = group

  override def visualizeGeneStats(geneStats: GeneStats,cName:String): Unit = geneStats match {
    case AllelicGeneStats(g,d,p,a,aq,f) => {
      setGeneDetails(
        chromosomeName= cName,
        geneId= g.geneId.mkString(","),
        alleleId= g.alleleCode.mkString(","),
        affectedQualities = aq.map(_.toString),
        features= featuresToString(f),
        dominanceLevel = d.toString,
        probability = p.toString,
        active = if(a) "Yes" else "No"
      )
    }
    case BasicGeneStats(g,i)=>
      setBasicGeneDetails(cName,g.geneId.mkString(","),i)
  }

  implicit class RichText(string:String){
    def setStyle(style:String):Text = {
      val text = new Text(string)
      text.setStyle(style)
      text.setFill(Color.web("67809F"))
      text.setFont(Font.font("Calibri", 24))
      text
    }
    def toText:Text = {
      val text = new Text(string)
      text.setFill(Color.White)
      text.setFont(Font.font("Calibri", 24))
      text
    }
  }
  private def setGeneDetails(chromosomeName: String, geneId: String,alleleId:String, affectedQualities: Seq[String],features:Seq[String], dominanceLevel: String, probability: String, active: String): Unit = {
    val style:String = "-fx-font-weight: 900"
    val allText:Seq[Text] = (chromosomeName+"\n").setStyle(style) ::
      "Gene Id: ".setStyle(style)::(geneId+"\n").toText   ::
      "Allele Id: ".setStyle(style)::(alleleId+"\n").toText ::
      "Affect:\n".setStyle(style)::(affectedQualities.toSet.mkString(", ")+"\n").toText ::
      "Features:\n".setStyle(style)::(features.mkString(",\n")+"\n").toText ::
      "Dominance Level: ".setStyle(style)::(dominanceLevel+"\n").toText ::
      "Probability: ".setStyle(style)::(probability+"\n").toText ::
      "Active: ".setStyle(style)::(active+"\n").toText :: List()
    textLabel.children.clear()
    textLabel.children.addAll(allText.asJava)
  }
  private def featuresToString(features: Seq[(String,Double)]):Seq[String] = {
    features.map(e =>e._1+": "+e._2)
  }
  private def setBasicGeneDetails(chromosomeName: String,geneId:String,identifiedThing:String): Unit ={
    val style:String = "-fx-font-weight: 900"
    val allText:Seq[Text]  = (chromosomeName+"\n").setStyle(style) ::
      "Gene Id: ".setStyle(style)::(geneId+"\n").toText ::
      "Identify: ".setStyle(style)::(identifiedThing+"\n").toText ::List()
    textLabel.children.clear()
    textLabel.children.addAll(allText.asJava)
  }

  override def emptyGeneStats(): Unit ={
    textLabel.children.clear()
    textLabel.children.add("EmptyGene".toText)
  }
}

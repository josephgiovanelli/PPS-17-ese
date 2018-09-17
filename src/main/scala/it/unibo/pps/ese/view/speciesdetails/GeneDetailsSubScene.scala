package it.unibo.pps.ese.view.speciesdetails
sealed trait Side
case object Left extends Side
case object Right extends Side

import it.unibo.pps.ese.genetics.dnaexpression.{AllelicGeneStats, BasicGeneStats, EmptyGeneStats, GeneStats}
import javafx.scene.text.Text
import scalafx.scene.layout.VBox
import scalafx.scene.text.TextFlow
import scalafx.scene.{Group, Parent, SceneAntialiasing, SubScene}
import scalaz._
import Scalaz._

trait GeneDetails{
  def visualizeGeneStats(geneStats: GeneStats,cName:String)
  def emptyGeneStats():Unit
}

class GeneDetailsSubScene(width:Double, height:Double, side: Side) extends SubScene(width,height,true,SceneAntialiasing.Balanced) with GeneDetails {
  import it.unibo.pps.ese.view.utilities.TextUtilities._
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
    vbox.setStyle(cssLayout)
    textLabel.setMaxWidth(width*0.75)
    vbox.getChildren.add(textLabel)

    vbox
  }

  val group = new Group()
  group.children.addAll(buildDetailsBox(textLabel))
  root = group

  override def visualizeGeneStats(geneStats: GeneStats,cName:String): Unit = geneStats match {
    case AllelicGeneStats(g,d,p,a,aq,f) =>
      setGeneDetails(
        geneType= g.geneType.toString,
        chromosomeName= cName,
        geneId= g.geneId.mkString(","),
        alleleId= g.alleleCode.mkString(","),
        affectedQualities = aq.map(_.toString),
        features= featuresToString(f),
        dominanceLevel = d.toString,
        probability = p.toString,
        active = a ? "Yes" | "No"
      )
    case BasicGeneStats(g,i)=>
      setBasicGeneDetails(
        chromosomeName = cName,
        geneType=g.geneType.toString,
        geneId=g.geneId.mkString(","),
        identifiedThing = i
      )
    case EmptyGeneStats()=>
      emptyGeneStats()
  }
  override def emptyGeneStats(): Unit ={
    textLabel.addAndClear("EmptyGene".toText)
  }

  private def setGeneDetails(chromosomeName: String,geneType:String, geneId: String,alleleId:String, affectedQualities: Seq[String],features:Seq[String], dominanceLevel: String, probability: String, active: String): Unit = {
    val allText:Seq[Text] =
      buildBasicTextInfoSeq(chromosomeName,geneType,geneId):::
      "Allele Id: ".toLabelText::(alleleId+"\n").toText ::
      "Affect:\n".toLabelText::(affectedQualities.toSet.mkString(", ")+"\n").toText ::
      "Features:\n".toLabelText::(features.mkString(",\n")+"\n").toText ::
      "Dominance Level: ".toLabelText::(dominanceLevel+"\n").toText ::
      "Probability: ".toLabelText::(probability+"\n").toText ::
      "Active: ".toLabelText::(active+"\n").toText :: List()
    textLabel.addAllAndClear(allText)
  }
  private def featuresToString(features: Seq[(String,Double)]):Seq[String] = {
    features.map(e =>e._1+": "+e._2)
  }
  private def setBasicGeneDetails(chromosomeName: String,geneType:String,geneId:String,identifiedThing:String): Unit ={
    val style:String = "-fx-font-weight: 900"
    val allText:Seq[Text]  =
        buildBasicTextInfoSeq(chromosomeName,geneType,geneId):::
        "Identify: ".toLabelText::(identifiedThing+"\n").toText::List()
    textLabel.addAllAndClear(allText)
  }

  private def buildBasicTextInfoSeq(chromosomeName: String,geneType:String,geneId:String):List[Text]= {
    (chromosomeName+"\n").toLabelText::
    geneType.toLabelText::("\n").toText ::
    "Gene Id: ".toLabelText::(geneId+"\n").toText :: List()
  }
}

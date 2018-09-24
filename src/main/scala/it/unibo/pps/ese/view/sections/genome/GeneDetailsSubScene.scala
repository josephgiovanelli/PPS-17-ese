package it.unibo.pps.ese.view.sections.genome
sealed trait Side
case object Left extends Side
case object Right extends Side

import it.unibo.pps.ese.model.genetics.dnaexpression.{AllelicGeneStats, BasicGeneStats, EmptyGeneStats, GeneStats}
import javafx.scene.text.Text
import scalafx.scene.layout.VBox
import scalafx.scene.text.TextFlow
import scalafx.scene.{Group, Parent, SceneAntialiasing, SubScene}
import scalaz._
import Scalaz._
/**
  * Side view to visualize information about the selected gene
  */
trait GeneDetails{
  /**
    * To visualize the information about the gene in argument
    * @param geneStats
    *                  The gene's informations
    * @param cName
    *              The chromosome to which the gene belongs
    */
  def visualizeGeneStats(geneStats: GeneStats,cName:String)

  /**
    * To clear the gene informations
    */
  def emptyGeneStats():Unit
}

/**
  * The [[SubScene]] to visualize the gene's details
  * @param width
  *              Width
  * @param height
  *              Height
  * @param side
  *             The [[Side]], that can be [[Left]] or [[Right]]
  */
class GeneDetailsSubScene(width:Double, height:Double, side: Side) extends SubScene(width,height,true,SceneAntialiasing.Balanced) with GeneDetails {
  import it.unibo.pps.ese.view.utilities.TextUtilities._
  import Texts._
  import GeneDetailsUtilities._
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
        geneId= g.geneId.mkString(comma),
        alleleId= g.alleleCode.mkString(comma),
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
        geneId=g.geneId.mkString(comma),
        identifiedThing = i
      )
    case EmptyGeneStats()=>
      emptyGeneStats()
  }

  override def emptyGeneStats(): Unit ={
    textLabel.addAndClear(emptyGeneText.toText)
  }

  /**
    * All the string that can be used in this subscene
    */
  private object Texts{
    val emptyGeneText = "EmptyGene"
    val alleleText = "Allele Id: "
    val affectText = "Affect:\n"
    val featuresText = "Features:\n"
    val dominanceText = "Dominance Level: "
    val probabilityText = "Probability: "
    val activeText = "Active: "
    val newLine = "\n"
    val identifyText = "Identify: "
    val comma = ","
    val space = " "
    val geneIdText = "Gene Id: "
  }

  /**
    * Some methods to visualize the information about the gene
    */
  private[this] object GeneDetailsUtilities{

    def setGeneDetails(chromosomeName: String,geneType:String, geneId: String,alleleId:String, affectedQualities: Seq[String],features:Seq[String], dominanceLevel: String, probability: String, active: String): Unit = {
      //All the text organized as a seq of Text, which can be added to the TextFlow
      val allText:Seq[Text] =
        buildBasicTextInfoSeq(chromosomeName,geneType,geneId):::
          alleleText.toLabelText::(alleleId+newLine).toText ::
          affectText.toLabelText::(affectedQualities.toSet.mkString(comma+space)+newLine).toText ::
          featuresText.toLabelText::(features.mkString(comma+newLine)+newLine).toText ::
          dominanceText.toLabelText::(dominanceLevel+newLine).toText ::
          probabilityText.toLabelText::(probability+newLine).toText ::
          activeText.toLabelText::(active+newLine).toText :: List()
      textLabel.addAllAndClear(allText)
    }
    def featuresToString(features: Seq[(String,Double)]):Seq[String] = {
      features.map(e =>e._1+": "+e._2)
    }
    def setBasicGeneDetails(chromosomeName: String,geneType:String,geneId:String,identifiedThing:String): Unit ={
      val style:String = "-fx-font-weight: 900"
      val allText:Seq[Text]  =
        buildBasicTextInfoSeq(chromosomeName,geneType,geneId):::
          identifyText.toLabelText::(identifiedThing+newLine).toText::List()
      textLabel.addAllAndClear(allText)
    }

    def buildBasicTextInfoSeq(chromosomeName: String,geneType:String,geneId:String):List[Text]= {
      (chromosomeName+newLine).toLabelText::
        geneType.toLabelText::(newLine).toText ::
        geneIdText.toLabelText::(geneId+newLine).toText :: List()
    }
  }
}

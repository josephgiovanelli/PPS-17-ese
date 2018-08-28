package it.unibo.pps.ese.view.speciesdetails
sealed trait Side
case object Left extends Side
case object Right extends Side

import it.unibo.pps.ese.view.speciesdetails.Example.setTitle
import javafx.scene.text.{Font, Text}
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.scene.{Group, Parent, SceneAntialiasing, SubScene}
trait GeneDetails{
  def setGeneDetails(
                    chromosomeName:String,
                    geneId:String,
                    alleleId:String,
                    affectedQualities:Seq[String],
                    dominanceLevel:String,
                    probability:String,
                    active:String)
}

class GeneDetailsScene(width:Double,height:Double,side: Side) extends SubScene(width,height,true,SceneAntialiasing.Balanced) with GeneDetails {
  private val textLabel = new Text("No gene selected")
  private[this]def buildDetailsBox(textLabel: Text):Parent= {
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
    textLabel.setFont(Font.font("Calibri", 24))
    textLabel.setFill(Color.web("67809F"))
    vbox.getChildren().add(textLabel)

    vbox
  }

  val group = new Group()
  group.children.addAll(buildDetailsBox(textLabel))
  root = group

  override def setGeneDetails(chromosomeName: String, geneId: String,alleleId:String, affectedQualities: Seq[String], dominanceLevel: String, probability: String, active: String): Unit = {
    val text = chromosomeName+"\n" +
      "Gene Id: "+geneId+"\n" +
      "Allele Id: "+alleleId+"\n" +
      "Affect: "+affectedQualities.mkString(",")+"\n" +
      "Dominance Level: "+dominanceLevel+"\n" +
      "Probability: "+probability+"%\n" +
      "Active: "+active+"\n"
    textLabel.setText(text)
  }
}

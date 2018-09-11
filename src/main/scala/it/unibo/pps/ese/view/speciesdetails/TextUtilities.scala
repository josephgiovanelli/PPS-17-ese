package it.unibo.pps.ese.view.speciesdetails

import javafx.scene.text.{Font, Text}
import scalafx.scene.Node
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color
import scalafx.scene.text.{TextAlignment, TextFlow}
import scalafx.Includes._
import scalafx.geometry.Pos
object TextUtilities {
  implicit class RichText(string:String){
    def toTextStyled(style:String,color: Color,font: Font):Text ={
      val text = new Text(string)
      text.setStyle(style)
      text.setFill(color)
      text.setFont(font)
      text
    }
    def toLabelText:Text = {
      toTextStyled(
        style="-fx-font-weight: 900",
        color=Color.web("67809F"),
        font=Font.font("Calibri", 24)
      )
    }
    def toText:Text = {
      toTextStyled(
        style = "",
        color = Color.White,
        font = Font.font("Calibri", 24)
      )
    }
    def toHBox:HBox = {
      val hBox = new HBox()
      val text:Text = toTextStyled(
        style = "",
        color = Color.Black,
        font = Font.font("Calibri", 24)
      )
      hBox.alignment = Pos.BaselineCenter
      text.textAlignment = TextAlignment.Center
      hBox.children += text
      hBox
    }
  }
  implicit class RichTextFlow(textFlow:TextFlow){
    import scala.collection.JavaConverters._
    def addAndClear(node:Text): Unit ={
      textFlow.children clear()
      textFlow.children add node
    }

    def addAllAndClear(nodes:Seq[Text]): Unit ={
      textFlow.children.clear()
      textFlow.children.addAll(nodes.asJava)
    }
  }
}

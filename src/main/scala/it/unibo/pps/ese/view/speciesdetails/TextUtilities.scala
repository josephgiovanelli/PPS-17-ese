package it.unibo.pps.ese.view.speciesdetails

import javafx.scene.text.{Font, Text}
import scalafx.scene.paint.Color

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
  }
}

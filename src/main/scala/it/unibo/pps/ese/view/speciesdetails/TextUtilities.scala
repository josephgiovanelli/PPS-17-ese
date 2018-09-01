package it.unibo.pps.ese.view.speciesdetails

import javafx.scene.text.{Font, Text}
import scalafx.scene.paint.Color

object TextUtilities {
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
}

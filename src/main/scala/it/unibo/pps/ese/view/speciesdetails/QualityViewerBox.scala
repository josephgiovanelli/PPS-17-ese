package it.unibo.pps.ese.view.speciesdetails

import scalafx.geometry.Pos
import scalafx.scene.control.{Label, ProgressBar}
import scalafx.scene.layout.HBox

sealed trait QualityViewer{
  def setQualityValue(value:Double)
}

class QualityViewerBox(val quality:String,val qualityValue:Double,style:String) extends HBox with QualityViewer {
  override def setQualityValue(value: Double): Unit = {
    bar.progress = value / 100.0
    valueLabel.setText(value.toString)
  }

  spacing = 30
  alignment = Pos.CenterLeft
  val bar: ProgressBar = new ProgressBar {
    maxWidth = 100
    visible = true
  }

  bar.progress = qualityValue / 100.0
  bar.prefWidth = 100
  bar.setStyle(style)
  val qualityLabel: Label = new Label(quality)
  qualityLabel.prefWidth = 120
  val valueLabel: Label = new Label()
  valueLabel.prefWidth = 50
  valueLabel.setText(qualityValue.toString)
  children = List(qualityLabel, bar, valueLabel)
}
class NonNumericQualityViewerBox(val value:String,style:String) extends HBox{
  val bar: ProgressBar = new ProgressBar {
    maxWidth = 100
    visible = true
    progress = 1
  }
  spacing = 30
  bar.setStyle(style)
  bar.prefWidth = 100
  val qualityLabel: Label = new Label(value)
  qualityLabel.prefWidth = 120
  children = List(qualityLabel,bar)

}
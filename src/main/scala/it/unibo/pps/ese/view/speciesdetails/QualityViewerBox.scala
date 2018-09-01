package it.unibo.pps.ese.view.speciesdetails

import scalafx.geometry.Pos
import scalafx.scene.control.{Label, ProgressBar}
import scalafx.scene.layout.HBox

sealed trait QualityViewer{
  def setQualityValue(value:Double)
}
class QualityViewerBox(val quality:String,val qualityValue:Double) extends HBox with QualityViewer{
  override def setQualityValue(value: Double): Unit ={
    bar.progress = value/100.0
    valueLabel.setText(value.toString)
  }

  spacing = 30
  alignment = Pos.CenterLeft
  val bar:ProgressBar = new ProgressBar{
    maxWidth = 100
    visible = true
  }
  bar.progress = qualityValue/100.0
  bar.prefWidth = 100
  val qualityLabel:Label = new Label(quality)
  qualityLabel.prefWidth = 120
  val valueLabel:Label = new Label()
  valueLabel.prefWidth = 50
  valueLabel.setText(qualityValue.toString)
  //      left = qualityLabel
  //      center = bar
  //      right = valueLabel
  children = List(qualityLabel, bar,valueLabel)
}
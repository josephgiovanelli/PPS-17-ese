package it.unibo.pps.ese.view.sections.genome

import scalafx.geometry.Pos
import scalafx.scene.control.{Label, ProgressBar}
import scalafx.scene.layout.HBox

/**
  * A [[HBox]] to visualize a quality and its value
  */
sealed trait QualityViewer extends HBox{
  def setQualityValue(value:Double)
}
object QualityBoxUtilities{
  implicit class QualityString(qualityString: String){
    def --->(value: Double):QualityViewerBox = {
      new QualityViewerBox(qualityString,value,"")
    }
  }
}

/**
  * A [[QualityViewer]] for qualities with numeric values
  * @param quality
  * @param qualityValue
  * @param style
  */
class QualityViewerBox(val quality:String,val qualityValue:Double,style:String) extends QualityViewer {
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

/**
  * A [[QualityViewer]] for qualities with non numeric values
  * @param value
  * @param style
  */
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
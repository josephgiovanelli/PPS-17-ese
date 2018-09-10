package it.unibo.pps.ese.utils

import scalafx.scene.image.Image

sealed trait ScalableImage{
  def scaleWidth(ratio:Double):Image
  def scaleHeight(ratio:Double):Image
  def scale(ratio:Double):Image
  def image:Image
}
object ScalableImage {
    def apply(url:String): ScalableImage = ScalableImageImpl(url)
    private case class ScalableImageImpl(url:String) extends ScalableImage{
      override def scaleWidth(ratio: Double): Image = {
        val newWidth = image.width.value * ratio
        val oldHeight = image.height.value
        new Image(url,newWidth,oldHeight,true,true)
      }

      override def scaleHeight(ratio: Double): Image = {
        val oldWidth = image.width.value
        val newHeight = image.height.value* ratio
        new Image(url,oldWidth,newHeight,true,true)
      }

      override def scale(ratio: Double): Image = {
        val newWidth = image.width.value * ratio
        val newHeight = image.height.value* ratio
        new Image(url,newWidth,newHeight,true,true)
      }

      override val image: Image = new Image(url)
    }
    implicit def toImage(scalableImage: ScalableImage):Image = scalableImage.image
    implicit class RichScalableImage(scalableImage: ScalableImage){
      def *(ratio:Double):Image  = scalableImage.scale(ratio)
    }
}

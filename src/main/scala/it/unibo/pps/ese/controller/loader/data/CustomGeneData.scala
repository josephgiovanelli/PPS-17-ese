package it.unibo.pps.ese.controller.loader.data

trait PartialCustomGeneData extends PartialGeneData {
  def getConversionMap: Option[Map[String, Map[String, Double]]]
}
trait CompleteCustomGeneData extends PartialCustomGeneData with CompleteGeneData {
  def conversionMap: Map[String, Map[String, Double]] = getConversionMap.getOrElse(throw new IllegalStateException())
}
package it.unibo.pps.ese.controller.simulation.loader.data

trait CustomGeneData[T <: PartialAlleleData] extends GeneData[T] {
  def getConversionMap: Option[Map[String, Map[String, Double]]]
}
trait FullCustomGeneData[T <: PartialAlleleData] extends CustomGeneData[T] with FullDefaultGeneData[T] {
  def conversionMap: Map[String, Map[String, Double]] = getConversionMap.getOrElse(throw new IllegalStateException())
}

object CustomGeneData {
  type PartialCustomGeneData = CustomGeneData[_ <: PartialAlleleData]
  trait CompleteCustomGeneData extends FullCustomGeneData[CompleteAlleleData]
}
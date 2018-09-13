package it.unibo.pps.ese.controller.loader.data


trait DefaultGeneData[T <: PartialAlleleData] extends GeneData[T]
trait FullDefaultGeneData[T <: PartialAlleleData] extends DefaultGeneData[T] with FullGeneData[T]

object DefaultGeneData {
  type PartialDefaultGeneData = DefaultGeneData[_ <: PartialAlleleData]
  type CompleteDefaultGeneData = PartialDefaultGeneData with FullDefaultGeneData[_ <: CompleteAlleleData]
}
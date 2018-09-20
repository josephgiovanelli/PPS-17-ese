package it.unibo.pps.ese.controller.simulation.loader.data


trait DefaultGeneData[T <: PartialAlleleData] extends GeneData[T]
trait FullDefaultGeneData[T <: PartialAlleleData] extends DefaultGeneData[T] with FullGeneData[T]

object DefaultGeneData {
  type PartialDefaultGeneData = DefaultGeneData[_ <: PartialAlleleData]
  trait CompleteDefaultGeneData extends FullDefaultGeneData[CompleteAlleleData]
}
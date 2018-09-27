package it.unibo.pps.ese.controller.simulation.loader.data

/** Trait that defines default gene's data. All non-mandatory fields can be not filled, so are represented by an option
  *
  * @tparam T Internal alleles' type(completion level)
  */
trait DefaultGeneData[T <: PartialAlleleData] extends GeneData[T]
/** Trait that defines full default gene's data, where all non-mandatory fields must be filled*/
trait FullDefaultGeneData[T <: PartialAlleleData] extends DefaultGeneData[T] with FullGeneData[T]

object DefaultGeneData {
  /** Type that defines partial default gene data's features*/
  type PartialDefaultGeneData = DefaultGeneData[_ <: PartialAlleleData]
  /** Trait that defines complete default gene data's features*/
  trait CompleteDefaultGeneData extends FullDefaultGeneData[CompleteAlleleData]
}
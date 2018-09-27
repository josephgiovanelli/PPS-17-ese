package it.unibo.pps.ese.controller.simulation.loader.data

/** Trait that defines custom gene's data. All non-mandatory fields can be not filled, so are represented by an option
  *
  * @tparam T Internal alleles' type(completion level)
  */
trait CustomGeneData[T <: PartialAlleleData] extends DefaultGeneData[T] {
  /** Gene's conversion map represented by a map containing properties as key and the respective conversion maps as value*/
  val getConversionMap: Option[Map[String, Map[String, Double]]]
}

/**Rich trait that defines full custom gene's data, where all non-mandatory fields must be filled. Trait can be directly
  * mixed with a [[it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData]]
  */
trait FullCustomGeneData[T <: PartialAlleleData] extends CustomGeneData[T] with FullDefaultGeneData[T] {
  /**
    * @throws IllegalStateException if property is not set
    * @return Gene's conversion map represented by a map containing properties as key and the respective conversion maps as value
    */
  @throws[IllegalStateException]
  def conversionMap: Map[String, Map[String, Double]] = getConversionMap.getOrElse(throw new IllegalStateException())
}

object CustomGeneData {
  /** Type that defines partial custom gene data's features*/
  type PartialCustomGeneData = CustomGeneData[_ <: PartialAlleleData]
  /** trait that defines complete custom gene data's features*/
  trait CompleteCustomGeneData extends FullCustomGeneData[CompleteAlleleData]
}
package it.unibo.pps.ese.controller.simulation.loader.data

import it.unibo.pps.ese.controller.simulation.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.simulation.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}

/** Trait that defines animal's data. All non-mandatory fields can be not filled, so are represented by an option
  *
  * @tparam C Internal custom chromosomes' type(completion level)
  * @tparam D Internal default chromosomes' type(completion level)
  */
trait AnimalData[+C<:PartialCustomGeneData, +D<:PartialDefaultGeneData] extends EntityData {
  /**Animal's typology*/
  val getTypology: Option[String]
  /**Animal's structural chromosome, as list of it's genes*/
  val getStructuralChromosome: Option[Iterable[C]]
  /**Animal's regulation chromosome, as list of it's genes*/
  val getRegulationChromosome: Option[Iterable[D]]
  /**Animal's sexual chromosome, as list of it's genes*/
  val getSexualChromosome: Option[Iterable[D]]
}

/**Rich trait that defines full animal's data, where all non-mandatory fields must be filled. Trait can be directly
  * mixed with a [[it.unibo.pps.ese.controller.simulation.loader.data.AnimalData]]
  */
trait FullAnimalData[C<:PartialCustomGeneData, D<:PartialDefaultGeneData] extends AnimalData[C, D] with FullEntityData{
  /**
    * @throws IllegalStateException if property is not set
    * @return Animal's typology
    */
  @throws[IllegalStateException]
  def typology: String = getTypology.getOrElse(throw new IllegalStateException())
  /**
    * @throws IllegalStateException if property is not set
    * @return Animal's structural chromosome, as list of it's genes
    */
  @throws[IllegalStateException]
  def structuralChromosome: Set[C] = getStructuralChromosome.getOrElse(throw new IllegalStateException()).toSet
  /**
    * @throws IllegalStateException if property is not set
    * @return Animal's regulation chromosome, as list of it's genes
    */
  @throws[IllegalStateException]
  def regulationChromosome: Set[D] = getRegulationChromosome.getOrElse(throw new IllegalStateException()).toSet
  /**
    * @throws IllegalStateException if property is not set
    * @return Animal's sexual chromosome, as list of it's genes
    */
  @throws[IllegalStateException]
  def sexualChromosome: Set[D] = getSexualChromosome.getOrElse(throw new IllegalStateException()).toSet
}

object AnimalData {
  /** Type that defines partial animal data's features*/
  type PartialAnimalData = AnimalData[_ <: PartialCustomGeneData, _ <: PartialDefaultGeneData]
  /** Trait that defines complete animal data's features*/
  trait CompleteAnimalData extends FullAnimalData[CompleteCustomGeneData, CompleteDefaultGeneData]
}

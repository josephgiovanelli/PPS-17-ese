package it.unibo.pps.ese.controller.loader.data

import it.unibo.pps.ese.controller.loader.data.CustomGeneData.{CompleteCustomGeneData, PartialCustomGeneData}
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.{CompleteDefaultGeneData, PartialDefaultGeneData}

trait AnimalData[C<:PartialCustomGeneData, D<:PartialDefaultGeneData] extends EntityData {
  def getTypology: Option[String]
  def getStructuralChromosome: Option[Iterable[C]]
  def getRegulationChromosome: Option[Iterable[D]]
  def getSexualChromosome: Option[Iterable[D]]
}

trait FullAnimalData[C<:PartialCustomGeneData, D<:PartialDefaultGeneData] extends AnimalData[C, D] with FullEntityData{
  def typology: String = getTypology.getOrElse(throw new IllegalStateException())
  def structuralChromosome: Set[C] = getStructuralChromosome.getOrElse(throw new IllegalStateException()).toSet
  def regulationChromosome: Set[D] = getRegulationChromosome.getOrElse(throw new IllegalStateException()).toSet
  def sexualChromosome: Set[D] = getSexualChromosome.getOrElse(throw new IllegalStateException()).toSet
}

object AnimalData {
  type PartialAnimalData = AnimalData[_ <: PartialCustomGeneData, _ <: PartialDefaultGeneData]
  trait CompleteAnimalData extends FullAnimalData[CompleteCustomGeneData, CompleteDefaultGeneData]
}

package it.unibo.pps.ese.model.genetics.entities

import it.unibo.pps.ese.controller.simulation.loader.data.CompletePlantData
import it.unibo.pps.ese.model.genetics.Utilities
import it.unibo.pps.ese.model.genetics.dna.Genome

/**
  *  The plant information
  */
trait PlantInfo extends EntityInfo{
  def genome:Genome
}
object PlantInfo{
  def apply(plantData: CompletePlantData,genome: Genome): PlantInfo = PlantInfoImpl(plantData,genome)

  def unapply(arg: PlantInfo): Option[(Species,Gender,Map[QualityType,Quality])] =
    Some(
      arg.species,
      arg.gender,
      arg.qualities
    )

  case class PlantInfoImpl(plantData: CompletePlantData,genome: Genome) extends PlantInfo{
    override val species: Species = Species(Plant,plantData.name)

    override val gender: Gender = Utilities.pickRandomElement(Male,Female)

    override val qualities: Map[QualityType, Quality] = Map(
      QualityType.Height |->| plantData.height,
      QualityType.NutritionalValue |->| plantData.nutritionalValue,
      QualityType.Hardness |->| plantData.hardness
    )
  }
}

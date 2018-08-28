package it.unibo.pps.ese.genetics.entities

import it.unibo.pps.ese.controller.loader.data.PlantData
import it.unibo.pps.ese.genetics.{Utilities, entities}
import it.unibo.pps.ese.genetics.dna.Genome

trait PlantInfo extends EntityInfo{
  def genome:Genome
}
object PlantInfo{
  def apply(plantData: PlantData,genome: Genome): PlantInfo = PlantInfoImpl(plantData,genome)
  case class PlantInfoImpl(plantData: PlantData,genome: Genome) extends PlantInfo{
    override val species: Species = Species(Plant,plantData.name)

    override val gender: Gender = Utilities.pickRandomElement(Male,Female)

    override val qualities: Map[QualityType, Quality] = Map(
      QualityType.Height |->| plantData.height,
      QualityType.NutritionalValue |->| plantData.nutritionalValue,
      QualityType.Attractiveness |->| plantData.attractiveness,
      QualityType.Hardness |->| plantData.hardness,
      QualityType.Availability |->| plantData.availability
    )
  }
}

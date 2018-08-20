package it.unibo.pps.ese.genetics.entities

import it.unibo.pps.ese.genetics.dna.AnimalGenome
import it.unibo.pps.ese.genetics.dnaexpression.{AllelicBehaviour, AnimalFeature}

trait AnimalInfo {
  def species:Species
  def gender:Gender
  def dietType:DietType
  def genome:AnimalGenome
  def animalQualities:Map[QualityType,Quality]
  def activeAlleles:Seq[AllelicBehaviour]
}
object AnimalInfo{
  def apply(species: Species,animalFeature: AnimalFeature,genome: AnimalGenome): AnimalInfo = AnimalInfoImpl(
    species = species,
    gender = animalFeature.gender,
    dietType = animalFeature.dietType,
    genome = genome,
    animalQualities = animalFeature.animalQualities,
    activeAlleles = animalFeature.activeAllelicStructure
  )
  //Anche altro apply
  case class AnimalInfoImpl(
    species: Species,
    gender: Gender,
    dietType: DietType,
    genome: AnimalGenome,
    animalQualities:Map[QualityType,Quality],
    activeAlleles:Seq[AllelicBehaviour]
  ) extends AnimalInfo{
    override def toString: String = species+", "+"Gender: "+gender+",Diet: "+dietType+", Qualities: "+animalQualities.toString()
  }
}

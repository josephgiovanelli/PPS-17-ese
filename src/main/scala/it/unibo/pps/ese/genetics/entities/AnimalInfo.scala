package it.unibo.pps.ese.genetics.entities

import it.unibo.pps.ese.genetics.dna.AnimalGenome
import it.unibo.pps.ese.genetics.dnaexpression.{AllelicBehaviour, AnimalFeature}
trait EntityInfo {
  def species:Species
  def gender:Gender
  def qualities:Map[QualityType,Quality]
}
trait AnimalInfo extends EntityInfo{
  def dietType:DietType
  def genome:AnimalGenome
  def qualities:Map[QualityType,Quality]
  def activeAlleles:Seq[AllelicBehaviour]
}

object AnimalInfo{
  def apply(species: Species,animalFeature: AnimalFeature,genome: AnimalGenome): AnimalInfo = AnimalInfoImpl(
    species = species,
    gender = animalFeature.gender,
    dietType = animalFeature.dietType,
    genome = genome,
    qualities = animalFeature.animalQualities,
    activeAlleles = animalFeature.activeAllelicStructure
  )

  def unapply(arg: AnimalInfo): Option[(Species,Gender,DietType,AnimalGenome,Map[QualityType,Quality],Seq[AllelicBehaviour])] = {
     Some(
       arg.species,
       arg.gender,
       arg.dietType,
       arg.genome,
       arg.qualities,
       arg.activeAlleles
     )
  }
  //Anche altro apply
  case class AnimalInfoImpl(
                             species: Species,
                             gender: Gender,
                             dietType: DietType,
                             genome: AnimalGenome,
                             qualities:Map[QualityType,Quality],
                             activeAlleles:Seq[AllelicBehaviour]
  ) extends AnimalInfo{
    override def toString: String = species+", "+"Gender: "+gender+",Diet: "+dietType+", Qualities: "+qualities.toString()
  }
}

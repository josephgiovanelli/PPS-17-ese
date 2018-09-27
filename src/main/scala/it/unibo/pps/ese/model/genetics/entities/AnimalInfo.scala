package it.unibo.pps.ese.model.genetics.entities

import it.unibo.pps.ese.model.genetics.dna.AnimalGenome
import it.unibo.pps.ese.model.genetics.dnaexpression.{AllelicBehaviour, AnimalFeature}

/**
  * Basic common information of an Entity
  */
trait EntityInfo {
  /**
    * @return a [[Species]] that is [[Reign]] + Species Name
    */
  def species:Species

  /**
    * @return The [[Gender]] of the entity
    */
  def gender:Gender
  def qualities:Map[QualityType,Quality]
}

/**
  * Information about Animal
  */
trait AnimalInfo extends EntityInfo{
  /**
    * @return [[Carnivorous]] or [[Herbivore]]
    */
  def dietType:DietType

  /**
    * @return The genome of an Animal
    */
  def genome:AnimalGenome

  /**
    * @return The alleles that express theme behaviour
    */
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

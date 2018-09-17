package it.unibo.pps.ese.view.filters

import it.unibo.pps.ese.entitybehaviors.LifePhases
import it.unibo.pps.ese.genericworld.model.ReignType
import it.unibo.pps.ese.genetics.entities.{DietType, Gender, Reign}
case class Range(lowValue:Double,highValue:Double)

sealed trait EntityFiltersValues{
  def reign: ReignType.Value
  def species: String
  def numericQualities:Map[String,Range]
}


case class PlantFiltersValues(
                               reign: ReignType.Value,
                               species:String,
                               numericQualities:Map[String,Range]
                             ) extends EntityFiltersValues
case class AnimalFiltersValues(
                                reign: ReignType.Value,
                                species:String,
                                gender:Gender,
                                diet: DietType,
                                lifePhase: LifePhases.Value,
                                numericQualities:Map[String,Range]
                              ) extends EntityFiltersValues

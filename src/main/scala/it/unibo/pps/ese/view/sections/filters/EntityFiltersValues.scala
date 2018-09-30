package it.unibo.pps.ese.view.sections.filters

import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.components.animals.LifePhases
import it.unibo.pps.ese.model.genetics.entities.{DietType, Gender, Reign}

/**
  * The range of the filter
  *
  * @param lowValue the low value specified
  * @param highValue the high value specified
  */
case class Range(lowValue:Double,highValue:Double)

/**
  * The entity's qualities which the filters are applied
  */
sealed trait EntityFiltersValues{

  /**
    *
    * @return the reign of the `Entity`
    */
  def reign: Option[ReignType.Value]

  /**
    *
    * @return the species of the `Entity`
    */
  def species: Option[String]

  /**
    *
    * @return the values range of a quality
    */
  def numericQualities:Map[String,Range]
}

/**
  * The filter's qualities of plants
  *
  * @param reign the reign
  * @param species the species
  * @param numericQualities the numeric qualities of a plant
  */
case class PlantFiltersValues(
                               reign: Option[ReignType.Value],
                               species:Option[String],
                               numericQualities:Map[String,Range]
                             ) extends EntityFiltersValues

/**
  * The filter's qualities of plants
  *
  * @param reign the reign
  * @param species the species
  * @param gender the gender
  * @param diet the diet
  * @param lifePhase the life phase
  * @param numericQualities the numeric qualities of an animal
  */
case class AnimalFiltersValues(
                                reign: Option[ReignType.Value],
                                species:Option[String],
                                gender:Option[Gender],
                                diet: Option[DietType],
                                lifePhase: Option[LifePhases.Value],
                                numericQualities:Map[String,Range]
                              ) extends EntityFiltersValues

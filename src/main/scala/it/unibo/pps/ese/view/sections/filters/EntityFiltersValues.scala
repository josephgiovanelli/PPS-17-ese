package it.unibo.pps.ese.view.sections.filters

import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.components.animals.LifePhases
import it.unibo.pps.ese.model.genetics.entities.{DietType, Gender, Reign}
case class Range(lowValue:Double,highValue:Double)

sealed trait EntityFiltersValues{
  def reign: Option[ReignType.Value]
  def species: Option[String]
  def numericQualities:Map[String,Range]
}


case class PlantFiltersValues(
                               reign: Option[ReignType.Value],
                               species:Option[String],
                               numericQualities:Map[String,Range]
                             ) extends EntityFiltersValues
case class AnimalFiltersValues(
                                reign: Option[ReignType.Value],
                                species:Option[String],
                                gender:Option[Gender],
                                diet: Option[DietType],
                                lifePhase: Option[LifePhases.Value],
                                numericQualities:Map[String,Range]
                              ) extends EntityFiltersValues

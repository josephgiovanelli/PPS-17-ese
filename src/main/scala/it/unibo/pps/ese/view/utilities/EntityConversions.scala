package it.unibo.pps.ese.view.utilities
import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo
import it.unibo.pps.ese.view.sections.filters.{AnimalFiltersValues, EntityFiltersValues, PlantFiltersValues, Range}
import scalaz._
import Scalaz._
import it.unibo.pps.ese.controller.simulation.runner.core.data.EntityInfo

/**
  *Utility object for entitie's qualities
  */
object EntityConversions {
  
  val strength: String = "Strength"
  val actionField: String = "Action Field"
  val visualField: String = "Visual Field"
  val attractiveness: String = "Attractiveness"
  val speed: String = "Speed"
  val fertility: String = "Fertility"
  val age: String = "Age"
  val averageLife: String = "Average Life"
  val percentageDecay: String = "Percentage Decay"
  val energy: String = "Energy"
  val energyRequirement: String = "Energy Requirement"
  val height: String = "Height"
  val nutritionalValue: String = "Nutritional Value"
  val defense: String = "Defense"
  val availability: String = "Availability"
  val hardness:String = "Hardness"

  /**
    * Implicit class à la "pimp library" to add methods to [[EntityInfo]] for visualize and filter animal's qualities
    * @param entityInfo
    */
  implicit class RichEntityInfo(entityInfo:EntityInfo){
    /**
      * To retrieve numeric qualities about an entity
      * @return
      */
    def numericQualities:Map[String,Double] = entityInfo.reign match {
      case ReignType.ANIMAL =>
        Map(
          strength->entityInfo.strength,
          actionField->entityInfo.actionField,
          visualField->entityInfo.visualField,
          attractiveness->entityInfo.attractiveness,
          speed->entityInfo.actualSpeed,
          fertility->entityInfo.actualFertility*100,
          age->entityInfo.age.toDouble*10,
          averageLife->entityInfo.averageLife,
          percentageDecay->entityInfo.percentageDecay*100,
          energy->(entityInfo.energy*0.1).toInt,
          energyRequirement->entityInfo.energyRequirements,
          height->entityInfo.height,
          nutritionalValue->entityInfo.nutritionalValue,
          defense->entityInfo.defense
        )
      case ReignType.PLANT =>
        Map(
          height->entityInfo.height,
          availability->entityInfo.nutritionalValue,
          hardness->entityInfo.defense
        )
    }

    /**
      * Method to filter an [[EntityInfo]]
      * @param entityFiltersValues
      * @return
      *         True if the entity info complies with the filters otherwise false
      */
    def applyFilter(entityFiltersValues: EntityFiltersValues):Boolean = {
      val commonFilter:EntityFiltersValues=>Boolean =
        filter=>{
          (if (filter.reign.isDefined) filter.reign.get== entityInfo.reign else true )&&
            (if (filter.species.isDefined) filter.species.get == entityInfo.baseEntityInfo.species.name else true) &&
            filter.numericQualities.forall {
              case (k, v) => (entityInfo.numericQualities(k) <= v.highValue || entityInfo.numericQualities(k) >100) &&
                entityInfo.numericQualities(k) >= v.lowValue
            }
          }

      entityFiltersValues match {
        case PlantFiltersValues(reign, species, numericQualities) =>
          commonFilter(entityFiltersValues)
        case AnimalFiltersValues(
          reign,
          species,
          gender,
          diet,
          lifePhase,
          numericQualities
        ) =>
            commonFilter(entityFiltersValues) &&
            (if (gender.isDefined) gender.get== entityInfo.baseEntityInfo.gender else true )&&
            (if (diet.isDefined) diet.get == entityInfo.baseEntityInfo.asInstanceOf[AnimalInfo].dietType else true) &&
            (if (lifePhase.isDefined) lifePhase.get== entityInfo.lifePhase else true )
      }
    }
  }
}

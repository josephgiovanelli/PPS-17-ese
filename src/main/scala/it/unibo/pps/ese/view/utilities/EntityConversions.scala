package it.unibo.pps.ese.view.utilities
import it.unibo.pps.ese.genericworld.model.{EntityInfo, ReignType}
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._
import it.unibo.pps.ese.genetics.entities.AnimalInfo
import it.unibo.pps.ese.view.filters.{AnimalFiltersValues, EntityFiltersValues, PlantFiltersValues, Range}
import scalaz._
import Scalaz._
object EntityConversions {
  implicit class RichEntityInfo(entityInfo:EntityInfo){
    def numericQualities:Map[String,Double] = entityInfo.reign match {
      case ReignType.ANIMAL =>
        Map(
          "Strong"->entityInfo.strong,
          "Action Field"->entityInfo.actionField,
          "Visual Field"->entityInfo.visualField,
          "Attractiveness"->entityInfo.attractiveness,
          "Speed"->entityInfo.actualSpeed,
          "Fertility"->entityInfo.fertility,
          "Age"->entityInfo.age.toDouble,
          "Average Life"->entityInfo.averageLife,
          "Percentage Decay"->entityInfo.percentageDecay,
          "Energy"->entityInfo.energy,
          "Energy Requirement"->entityInfo.energyRequirements,
          "Height"->entityInfo.height,
          "Nutritional Value"->entityInfo.nutritionalValue,
          "Defense"->entityInfo.defense
        )
      case ReignType.PLANT =>
        Map(
          "Height"->entityInfo.height,
          "Nutritional Value"->entityInfo.nutritionalValue,
          //    "Attractiveness"->entityDetails.attractiveness,
          //    "Hardness"->entityDetails.strong,
          "Availability"->entityInfo.availability
        )
    }
    def applyFilter(entityFiltersValues: EntityFiltersValues):Boolean = {
      val commonFilter:EntityFiltersValues=>Boolean =
        filter=>
            (filter.reign.isDefined? filter.reign.get == entityInfo.reign | true )&&
            (filter.species.isDefined?filter.species.get == entityInfo.baseEntityInfo.species.name|true )&&
            filter.numericQualities.forall {
            case (k, v) => entityInfo.numericQualities(k) < v.highValue &&
              entityInfo.numericQualities(k) > v.lowValue
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
            gender.isDefined?gender.get == entityInfo.baseEntityInfo.gender|true &&
            diet.isDefined?diet.get == entityInfo.baseEntityInfo.asInstanceOf[AnimalInfo].dietType|true &&
            lifePhase.isDefined?lifePhase == entityInfo.lifePhase | true
      }
    }
  }
}

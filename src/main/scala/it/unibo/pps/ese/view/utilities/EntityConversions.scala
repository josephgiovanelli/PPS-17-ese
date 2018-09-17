package it.unibo.pps.ese.view.utilities
import it.unibo.pps.ese.genericworld.model.{EntityInfo, ReignType}
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._
import it.unibo.pps.ese.genetics.entities.AnimalInfo
import it.unibo.pps.ese.view.filters.{AnimalFiltersValues, EntityFiltersValues, PlantFiltersValues, Range}

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
    def applyFilter(entityFiltersValues: EntityFiltersValues):Boolean
    = entityFiltersValues match {
      case PlantFiltersValues(reign,species,numericQualities)=>
        reign == entityInfo.reign &&
        species == entityInfo.baseEntityInfo.species.name &&
        entityFiltersValues.numericQualities.forall{
          case (k,v) => entityInfo.numericQualities(k) < v.highValue &&
                        entityInfo.numericQualities(k) > v.lowValue
        }
      case AnimalFiltersValues(
        reign,
        species,
        gender,
        diet,
        lifePhase,
        numericQualities
      ) =>
          reign == entityInfo.reign &&
          species == entityInfo.baseEntityInfo.species.name &&
          gender == entityInfo.baseEntityInfo.gender &&
          diet == entityInfo.baseEntityInfo.asInstanceOf[AnimalInfo].dietType &&
          lifePhase == entityInfo.lifePhase &&
          entityFiltersValues.numericQualities.forall{
            case (k,v) => entityInfo.numericQualities(k) < v.highValue &&
              entityInfo.numericQualities(k) > v.lowValue
          }
    }
  }
}

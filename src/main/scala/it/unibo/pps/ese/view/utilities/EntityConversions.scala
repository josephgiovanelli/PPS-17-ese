package it.unibo.pps.ese.view.utilities
import it.unibo.pps.ese.genericworld.model.{EntityInfo, ReignType}
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._
import it.unibo.pps.ese.genetics.entities.AnimalInfo
import it.unibo.pps.ese.view.filters.{AnimalFiltersValues, EntityFiltersValues, PlantFiltersValues, Range}
import scalaz._
import Scalaz._
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

  implicit class RichEntityInfo(entityInfo:EntityInfo){
    def numericQualities:Map[String,Double] = entityInfo.reign match {
      case ReignType.ANIMAL =>
//        println(entityInfo.fertility)
        Map(
          strength->entityInfo.strong,
          actionField->entityInfo.actionField,
          visualField->entityInfo.visualField,
          attractiveness->entityInfo.attractiveness,
          speed->entityInfo.actualSpeed,
          fertility->entityInfo.actualFertility*100,
          age->entityInfo.age.toDouble,
          averageLife->entityInfo.averageLife,
          percentageDecay->entityInfo.percentageDecay,
          energy->(entityInfo.energy*0.1).toInt,
          energyRequirement->entityInfo.energyRequirements,
          height->entityInfo.height,
          nutritionalValue->entityInfo.nutritionalValue,
          defense->entityInfo.defense
        )
      case ReignType.PLANT =>
        Map(
          height->entityInfo.height,
          nutritionalValue->entityInfo.nutritionalValue,
          //    "Attractiveness"->entityDetails.attractiveness,
          //    "Hardness"->entityDetails.strong,
          availability->entityInfo.availability
        )
    }
    def applyFilter(entityFiltersValues: EntityFiltersValues):Boolean = {
      val commonFilter:EntityFiltersValues=>Boolean =
        filter=>{
//          println(if (filter.reign.isDefined) filter.reign.get== entityInfo.reign else true )
//          println(if (filter.reign.isDefined) filter.reign.get== entityInfo.reign else true )
          (if (filter.reign.isDefined) filter.reign.get== entityInfo.reign else true )&&
            (if (filter.species.isDefined) filter.species.get == entityInfo.baseEntityInfo.species.name else true) &&
            filter.numericQualities.forall {
              case (k, v) => entityInfo.numericQualities(k) <= 10000 &&
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

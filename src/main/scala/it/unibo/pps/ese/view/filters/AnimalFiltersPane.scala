package it.unibo.pps.ese.view.filters

import it.unibo.pps.ese.entitybehaviors.LifePhases
import it.unibo.pps.ese.genericworld.model.{EntityInfoConversion, EntityState, ReignType}
import it.unibo.pps.ese.genetics.entities._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersHBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersVBoxes._
import it.unibo.pps.ese.view.utilities.EntityConversions
import it.unibo.pps.ese.view.utilities.EntityConversions._

trait AnimalFiltersPane extends FiltersVBox with DisablePane {
  def entityFiltersValues: EntityFiltersValues
  def updateFilters()
}

object AnimalFiltersPane {

  def apply(geneticsSimulator: GeneticsSimulator): AnimalFiltersPane =
    new AnimalFiltersPaneImpl(geneticsSimulator)

  class AnimalFiltersPaneImpl(geneticsSimulator: GeneticsSimulator) extends AnimalFiltersPane {

    val maleRepresentation: String = "Male"
    val femaleRepresentation: String = "Female"
    val childRepresentation: String = "Child"
    val adultRepresentation: String = "Adult"
    val elderlyRepresentation: String = "Elderly"
    val herbivoreRepresentation: String = "Herbivore"
    val carnivorusRepresentation: String = "Carnivorus"

    var speciesHBox: ChoiceHBox = choiceHBox("Species", geneticsSimulator.speciesList)
    val genderHBox: ChoiceHBox = choiceHBox("Gender",
      maleRepresentation ::
      femaleRepresentation ::
      List())
    val lifePhaseHBox: ChoiceHBox = choiceHBox("Life Phase",
      childRepresentation ::
      adultRepresentation ::
      elderlyRepresentation ::
      List())
    val dietTypeHBox: ChoiceHBox = choiceHBox("Diet Type",
      herbivoreRepresentation ::
      carnivorusRepresentation ::
      List())
    val strengthVBox: SliderVBox = sliderVBox("Strength")
    val actionFieldVBox: SliderVBox = sliderVBox("Action Field")
    val visualVBox: SliderVBox = sliderVBox("Visual Field")
    val attractivenessVBox: SliderVBox = sliderVBox("Attractiveness")
    val speedVBox: SliderVBox = sliderVBox("Speed")
    val fertilityVBox: SliderVBox = sliderVBox("Fertility")
    val ageVBox: SliderVBox = sliderVBox("Age")
    val averageLifeVBox: SliderVBox = sliderVBox("Average Life")
    val percentageDecayVBox: SliderVBox = sliderVBox("Percentage Decay")
    val energyVBox: SliderVBox = sliderVBox("Energy")
    val energyRequirementVBox: SliderVBox = sliderVBox("Energy Requirement")
    val heightVBox: SliderVBox = sliderVBox("Height")
    val nutritionalValueVBox: SliderVBox = sliderVBox("Nutritional Value")
    val defenseVBox: SliderVBox = sliderVBox("Defense")


    implicit class RichGender(gender: Gender) {
      def visualRepresentation: String = gender match {
        case Male => maleRepresentation
        case Female => femaleRepresentation
      }
    }

    implicit class RichLifePhase(lifePhases: LifePhases.Value) {
      def visualRepresentation: String = lifePhases match {
        case LifePhases.CHILD => childRepresentation
        case LifePhases.ADULT => adultRepresentation
        case LifePhases.ELDERLY => elderlyRepresentation
      }
    }

    implicit class RichDietType(dietType: DietType) {
      def visualRepresentation: String = dietType match {
        case Herbivore => herbivoreRepresentation
        case Carnivorous => carnivorusRepresentation
      }
    }

    implicit def stringToGender(string: String): Option[Gender] = string match {
      case `maleRepresentation` => Some(Male)
      case `femaleRepresentation` => Some(Female)
      case `emptyItem` => None
    }

    implicit def stringToDietType(string: String): Option[DietType] = string match {
      case `herbivoreRepresentation` => Some(Herbivore)
      case `carnivorusRepresentation` => Some(Carnivorous)
      case `emptyItem` => None
    }

    implicit def stringToLifePhases(string: String): Option[LifePhases.Value] = string match {
      case `childRepresentation` => Some(LifePhases.CHILD)
      case `adultRepresentation` => Some(LifePhases.ADULT)
      case `elderlyRepresentation` => Some(LifePhases.ELDERLY)
      case `emptyItem` => None
    }

    children =
      speciesHBox ::
      genderHBox ::
      lifePhaseHBox ::
      dietTypeHBox ::
      strengthVBox ::
      actionFieldVBox ::
      visualVBox ::
      attractivenessVBox ::
      speedVBox ::
      fertilityVBox ::
      ageVBox ::
      averageLifeVBox ::
      percentageDecayVBox ::
      energyVBox ::
      energyRequirementVBox ::
      heightVBox ::
      nutritionalValueVBox ::
      defenseVBox ::
      List()

    override def disableComponents(): Unit = {
      speciesHBox.disableComponents()
      genderHBox.disableComponents()
      lifePhaseHBox.disableComponents()
      dietTypeHBox.disableComponents()
      strengthVBox.disableComponents()
      actionFieldVBox.disableComponents()
      visualVBox.disableComponents()
      attractivenessVBox.disableComponents()
      speedVBox.disableComponents()
      fertilityVBox.disableComponents()
      ageVBox.disableComponents()
      averageLifeVBox.disableComponents()
      percentageDecayVBox.disableComponents()
      energyVBox.disableComponents()
      energyRequirementVBox.disableComponents()
      heightVBox.disableComponents()
      nutritionalValueVBox.disableComponents()
      defenseVBox.disableComponents()
    }

    override def enableComponents(): Unit = {
      speciesHBox.enableComponents()
      genderHBox.enableComponents()
      lifePhaseHBox.enableComponents()
      dietTypeHBox.enableComponents()
      strengthVBox.enableComponents()
      actionFieldVBox.enableComponents()
      visualVBox.enableComponents()
      attractivenessVBox.enableComponents()
      speedVBox.enableComponents()
      fertilityVBox.enableComponents()
      ageVBox.enableComponents()
      averageLifeVBox.enableComponents()
      percentageDecayVBox.enableComponents()
      energyVBox.enableComponents()
      energyRequirementVBox.enableComponents()
      heightVBox.enableComponents()
      nutritionalValueVBox.enableComponents()
      defenseVBox.enableComponents()
    }

    override def entityFiltersValues: EntityFiltersValues = AnimalFiltersValues(
      Some(ReignType.ANIMAL),
      speciesHBox.selectedItem match {
        case `emptyItem` => None
        case x => Some(x)
      },
      genderHBox.selectedItem,
      lifePhaseHBox.selectedItem,
      dietTypeHBox.selectedItem,
      Map[String, Range](
        strength->Range(strengthVBox.lowValue, strengthVBox.highValue),
        actionField->Range(actionFieldVBox.lowValue, actionFieldVBox.highValue),
        visualField->Range(visualVBox.lowValue, visualVBox.highValue),
        attractiveness->Range(attractivenessVBox.lowValue, attractivenessVBox.highValue),
        speed->Range(speedVBox.lowValue, speedVBox.highValue),
        fertility->Range(fertilityVBox.lowValue, fertilityVBox.highValue),
        age->Range(ageVBox.lowValue, ageVBox.highValue),
        averageLife->Range(averageLifeVBox.lowValue, averageLifeVBox.highValue),
        percentageDecay->Range(percentageDecayVBox.lowValue, percentageDecayVBox.highValue),
        energy->Range(energyVBox.lowValue, energyVBox.highValue),
        energyRequirement->Range(energyRequirementVBox.lowValue, energyRequirementVBox.highValue),
        EntityConversions.height->Range(heightVBox.lowValue, heightVBox.highValue),
        nutritionalValue->Range(nutritionalValueVBox.lowValue, nutritionalValueVBox.highValue),
        defense->Range(defenseVBox.lowValue, defenseVBox.highValue),
      )
    )

    override def updateFilters(): Unit = speciesHBox = choiceHBox("Species", geneticsSimulator.speciesList)
  }
}


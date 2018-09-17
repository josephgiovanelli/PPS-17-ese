package it.unibo.pps.ese.view.filters

import it.unibo.pps.ese.genericworld.model.{EntityInfoConversion, EntityState}
import it.unibo.pps.ese.genetics.entities._
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersHBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersVBoxes._

trait AnimalFiltersPane extends FiltersVBox with DisablePane

object AnimalFiltersPane {

  def apply(geneticsSimulator: GeneticsSimulator): AnimalFiltersPane =
    new AnimalFiltersPaneImpl(geneticsSimulator)

  class AnimalFiltersPaneImpl(geneticsSimulator: GeneticsSimulator) extends AnimalFiltersPane {
    val speciesHBox: ChoiceHBox = choiceHBox("Species", geneticsSimulator.speciesList)
    val genderHBox: ChoiceHBox = choiceHBox("Gender", "Male" :: "Female" :: List())
    val lifePhaseHBox: ChoiceHBox = choiceHBox("Life Phase", "Child" :: "Adult" :: "Elderly" :: List())
    val dietTypeHBox: ChoiceHBox = choiceHBox("Diet Type", "Herbivorus" :: "Carnivorus" :: List())
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
  }
}


package it.unibo.pps.ese.view.filters

import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersHBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersVBoxes._

trait AnimalFiltersPane extends FiltersVBox

object AnimalFiltersPane {

  def apply(geneticsSimulator: GeneticsSimulator): AnimalFiltersPane =
    new AnimalFiltersPaneImpl(geneticsSimulator)

  class AnimalFiltersPaneImpl(geneticsSimulator: GeneticsSimulator) extends AnimalFiltersPane {
    val speciesHBox: ChoiceHBox = choiceHBox("Species", geneticsSimulator.speciesList)
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
  }
}


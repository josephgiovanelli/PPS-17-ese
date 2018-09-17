package it.unibo.pps.ese.view.filters

import com.sun.javafx.binding.BidirectionalBinding.StringConversionBidirectionalBinding
import it.unibo.pps.ese.genericworld.model.ReignType
import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.WorldPane
import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.text.Text
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersLabels._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersHBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersVBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersChoiceBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersSliders._
import javafx.beans.binding.Bindings
import javafx.beans.value.ObservableStringValue
import scalafx.beans.property.{DoubleProperty, ReadOnlyStringProperty, StringProperty}
import scalafx.collections.{ObservableArray, ObservableBuffer}
import scalafx.scene.control.{ChoiceBox, Tooltip}
import scalafx.util.StringConverter
import scalafx.util.converter.NumberStringConverter
import it.unibo.pps.ese.view.utilities.EntityConversions
import it.unibo.pps.ese.view.utilities.EntityConversions._
import it.unibo.pps.ese.view.filters.PlantFiltersValues

trait PlantFiltersPane extends FiltersVBox with DisablePane {
  def entityFiltersValues: EntityFiltersValues
}

object PlantFiltersPane {

  def apply(geneticsSimulator: GeneticsSimulator): PlantFiltersPane =
    new PlantFiltersPaneImpl(geneticsSimulator)

  private class PlantFiltersPaneImpl(geneticsSimulator: GeneticsSimulator) extends PlantFiltersPane {

    val speciesHBox: ChoiceHBox = choiceHBox("Species", geneticsSimulator.plantSpeciesList)
    val heightVBox: SliderVBox = sliderVBox(EntityConversions.height)
    val nutritionalValueVBox: SliderVBox = sliderVBox(nutritionalValue)
    val availabilityVBox: SliderVBox = sliderVBox(availability)

    children =
      speciesHBox ::
      heightVBox ::
      nutritionalValueVBox ::
      availabilityVBox ::
      List()

    override def disableComponents(): Unit = {
      speciesHBox.disableComponents()
      heightVBox.disableComponents()
      nutritionalValueVBox.disableComponents()
      availabilityVBox.disableComponents()
    }

    override def enableComponents(): Unit = {
      speciesHBox.enableComponents()
      heightVBox.enableComponents()
      nutritionalValueVBox.enableComponents()
      availabilityVBox.enableComponents()
    }

    override def entityFiltersValues: EntityFiltersValues = PlantFiltersValues(
      Some(ReignType.PLANT),
      speciesHBox.selectedItem match {
        case `emptyItem` => None
        case x => Some(x)
      },
      Map[String, Range](
        EntityConversions.height->Range(heightVBox.lowValue, heightVBox.highValue),
        nutritionalValue->Range(nutritionalValueVBox.lowValue, nutritionalValueVBox.highValue),
        availability->Range(availabilityVBox.lowValue, availabilityVBox.highValue)
      )
    )
  }
}

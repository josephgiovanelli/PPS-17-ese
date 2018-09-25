package it.unibo.pps.ese.view.sections.filters

import com.sun.javafx.binding.BidirectionalBinding.StringConversionBidirectionalBinding
import it.unibo.pps.ese.model.genetics.GeneticsSimulator

import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.text.Text
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersLabels._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersHBoxes._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersVBoxes._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersChoiceBoxes._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersSliders._
import javafx.beans.binding.Bindings
import javafx.beans.value.ObservableStringValue

import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType

import scalafx.beans.property.{DoubleProperty, ReadOnlyStringProperty, StringProperty}
import scalafx.collections.{ObservableArray, ObservableBuffer}
import scalafx.scene.control.{ChoiceBox, Tooltip}
import scalafx.util.StringConverter
import scalafx.util.converter.NumberStringConverter
import it.unibo.pps.ese.view.utilities.EntityConversions
import it.unibo.pps.ese.view.utilities.EntityConversions._
import it.unibo.pps.ese.view.sections.filters.PlantFiltersValues
import it.unibo.pps.ese.view.main.WorldPane

object PlantFiltersPane {

  def apply(geneticsSimulator: GeneticsSimulator): EntityFiltersPane =
    new PlantFiltersPaneImpl(geneticsSimulator)

  private class PlantFiltersPaneImpl(geneticsSimulator: GeneticsSimulator) extends EntityFiltersPane {

    val speciesHBox: ChoiceHBox = choiceHBox("Species", geneticsSimulator.plantSpeciesList)
    val heightVBox: SliderVBox = sliderVBox(EntityConversions.height)
    val availabilityVBox: SliderVBox = sliderVBox(availability)

    children =
      speciesHBox ::
      heightVBox ::
      availabilityVBox ::
      List()

    override def disableComponents(): Unit = {
      speciesHBox.disableComponents()
      heightVBox.disableComponents()
      availabilityVBox.disableComponents()
    }

    override def enableComponents(): Unit = {
      speciesHBox.enableComponents()
      heightVBox.enableComponents()
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
        availability->Range(availabilityVBox.lowValue, availabilityVBox.highValue)
      )
    )

    override def updateFilters(): Unit = speciesHBox.setItems(geneticsSimulator.plantSpeciesList)

  }
}

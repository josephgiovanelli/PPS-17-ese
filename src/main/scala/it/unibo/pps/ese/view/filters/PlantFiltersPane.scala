package it.unibo.pps.ese.view.filters

import com.sun.javafx.binding.BidirectionalBinding.StringConversionBidirectionalBinding
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

trait PlantFiltersPane extends FiltersVBox

object PlantFiltersPane {

  def apply(geneticsSimulator: GeneticsSimulator): PlantFiltersPane =
    new PlantFiltersPaneImpl(geneticsSimulator)

  private class PlantFiltersPaneImpl(geneticsSimulator: GeneticsSimulator) extends PlantFiltersPane {

    val speciesLabel: FiltersLabel = normalLabel("Species")
    val speciesChoiceBox: FiltersChoiceBox[String] = defaultChoiceBox
    speciesChoiceBox.items = ObservableBuffer(geneticsSimulator.plantSpeciesList)
    val speciesHBox: FiltersHBox = componentsHBox
    speciesHBox.children += speciesLabel
    speciesHBox.children += speciesChoiceBox

    val heightLabel: FiltersLabel = normalLabel("Height")
    val heightHBox: FiltersHBox = componentsHBox
    val heightRangeSlider: FiltersRangeSlider = defaultRangeSlider
    val tooltip = Tooltip(heightRangeSlider.getLowValue.toString)

//    heightRangeSlider.setTooltip(tooltip)
//    heightRangeSlider.setOnMouseDragged(e => {
//      tooltip.text = heightRangeSlider.getLowValue.toInt.toString
//      tooltip.show(this, e.getScreenX, e.getSceneY)
//    })

    heightHBox.children += heightLabel
    heightHBox.children += heightRangeSlider

    children += speciesHBox
    children += heightHBox

  }
}

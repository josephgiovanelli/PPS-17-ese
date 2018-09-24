package it.unibo.pps.ese.view.sections.filters

import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersVBoxes._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersHBoxes._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersLabels._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersRadioButtons._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersSerparators._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersBorderPanes._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersHBoxes.FiltersHBox
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersBorderPanes._
import it.unibo.pps.ese.view.main.WorldPane

import scalafx.Includes._
import scalafx.beans.property.BooleanProperty
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.control.ScrollPane.ScrollBarPolicy
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.paint.Color

/**
  * A pane to choose the values of the filters
  */
trait FiltersPane extends ScrollPane {

  /**
    *
    * @return if the filters are active
    */
  def filtersOn: BooleanProperty

  /**
    * Updates the filters
    */
  def updateFilters()
}

object FiltersPane {

  def apply(worldPane: WorldPane, geneticsSimulator: GeneticsSimulator): FiltersPane = new FiltersPaneImpl(worldPane, geneticsSimulator)

  private class FiltersPaneImpl(worldPane: WorldPane, geneticsSimulator: GeneticsSimulator) extends FiltersPane {

    var filtersOn: BooleanProperty = BooleanProperty(false)

    val mainPane: BorderPane = new BorderPane()
    mainPane.background = new Background(Array(new BackgroundFill(Color.color(0.2, 0.2, 0.2, 1.0), CornerRadii.Empty, Insets.Empty)))

    val mainBox: FiltersVBox = defaultVBox
    mainBox.padding = Insets(30, 50, 30, 50)

    mainBox.prefWidth <== width
    mainBox.prefHeight <== height

    val reignLabel: FiltersLabel = bigLabel("Reign")
    val reignToggleGroup = new ToggleGroup()
    val animalRadio: FiltersRadioButton = defaultRadioButton("Animal")
    animalRadio.toggleGroup = reignToggleGroup
    val plantRadio: FiltersRadioButton = defaultRadioButton("Plant")
    plantRadio.toggleGroup = reignToggleGroup
    plantRadio.selected = true

    val reignRadioBox: FiltersVBox = componentsVBox
    reignRadioBox.children = plantRadio :: animalRadio :: List()

    val valuesContainerPane: FiltersBorderPane = defaultBorderPane
    val plantVBox: PlantFiltersPane = PlantFiltersPane(geneticsSimulator)
    val animalVBox: AnimalFiltersPane = AnimalFiltersPane(geneticsSimulator)
    valuesContainerPane.center = plantVBox

    val buttonsHBox: FiltersHBox = defaultHBox
    buttonsHBox.margin = Insets(30, 0, 0, 0)
    val applyButton: Button = new Button("Apply")
    applyButton.margin = Insets(0, 40, 0, 20)
    val clearButton: Button = new Button("Clear")
    clearButton.margin = Insets(0, 20, 0, 20)

    buttonsHBox.children = applyButton :: clearButton :: List()

    plantRadio.onAction = (e: ActionEvent) => {
      valuesContainerPane.center = plantVBox
    }
    animalRadio.onAction = (e: ActionEvent) => {
      valuesContainerPane.center = animalVBox
    }

    mainBox.children =
      reignLabel ::
      reignRadioBox ::
      defaultSeparator ::
      valuesContainerPane ::
      defaultSeparator ::
      buttonsHBox ::
      List()


    mainPane.center = mainBox
    content = mainPane

    hbarPolicy = ScrollBarPolicy.Never

    filtersOn <== applyButton.disabled

    applyButton.onAction = (e: ActionEvent) => {
      applyButton.disable = true
      clearButton.disable = false
      plantRadio.disable = true
      animalRadio.disable = true
      if (plantRadio.isSelected) {
        plantVBox.disableComponents()
        worldPane.applyFilters(plantVBox.entityFiltersValues)
      } else {
        animalVBox.disableComponents()
        worldPane.applyFilters(animalVBox.entityFiltersValues)
      }
    }

    clearButton.onAction = (e: ActionEvent) => {
      applyButton.disable = false
      clearButton.disable = true
      plantRadio.disable = false
      animalRadio.disable = false
      if (plantRadio.isSelected) {
        plantVBox.enableComponents()
      } else {
        animalVBox.enableComponents()
      }
      worldPane.clearFilters()
    }

    override def updateFilters(): Unit = {
      animalVBox.updateFilters()
      plantVBox.updateFilters()
    }
  }

}

/**
  * A pane that with mechanism to disable internal components
  */
trait DisablePane {

  /**
    * Disables its internal components
    */
  def disableComponents()

  /**
    * enables its internal components
    */
  def enableComponents()
}


package it.unibo.pps.ese.view.filters

import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersVBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersHBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersLabels._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersRadioButtons._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersSerparators._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory._
import it.unibo.pps.ese.view.WorldPane
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersHBoxes.FiltersHBox
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.control.ScrollPane.ScrollBarPolicy
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.paint.Color

trait FiltersPane extends ScrollPane

object FiltersPane {

  def apply(worldPane: WorldPane, geneticsSimulator: GeneticsSimulator): FiltersPane = new FiltersPaneImpl(worldPane, geneticsSimulator)

  private class FiltersPaneImpl(worldPane: WorldPane, geneticsSimulator: GeneticsSimulator) extends FiltersPane {

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

    val valuesContainerPane: BorderPane = new BorderPane()
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

  }

}


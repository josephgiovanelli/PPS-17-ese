package it.unibo.pps.ese.view.filters

import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersVBoxes._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersLabels._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersRadioButtons._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersSerparators._

import it.unibo.pps.ese.view.filters.FiltersComponentsFactory._
import it.unibo.pps.ese.view.WorldPane
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersHBoxes.FiltersHBox
import scalafx.Includes._
import scalafx.geometry.Insets
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

    val reignRadioBox: FiltersVBox = componentsVBox
    reignRadioBox.children = plantRadio :: animalRadio :: List()

    val plantVBox: FiltersVBox = PlantFiltersPane(geneticsSimulator)

    mainBox.children = reignLabel :: reignRadioBox :: defaultSeparator :: List()
    mainBox.children += plantVBox


    mainPane.center = mainBox
    content = mainPane


  }

}


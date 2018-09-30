package it.unibo.pps.ese.view.sections.filters

import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersChoiceBoxes.{FiltersChoiceBox, defaultChoiceBox}
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersHBoxes.FiltersHBox
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersLabels.FiltersLabel
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersSliders._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersLabels._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersVBoxes._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersHBoxes._
import it.unibo.pps.ese.view.sections.filters.FiltersComponentsFactory.FiltersSerparators._
import org.controlsfx.control.RangeSlider
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.text.TextAlignment

/**
  * A factory for filters' section custom components
  */
object FiltersComponentsFactory {

  private val backgroundColor: Color = Color.White

  private val LabelBigFontSize: Int = 30
  private val LabelNormalFontSize: Int = 22
  private val LabelSmallFontSize: Int = 15

  private val MinSliderValue = 0
  private val MaxSliderValue = 100


  object FiltersLabels {

    /**
      * A custom `Label` for filters' section
      */
    trait FiltersLabel extends Label {
      textFill = Color.White
    }

    private class LabelImpl(t: String, fontSize: Int) extends FiltersLabel {
      text = t
      style = s"-fx-font-size: $fontSize"
    }

    /**
      * Creates a small label
      *
      * @param text the label text
      * @return a small `FiltersLabel`
      */
    def smallLabel(text: String): FiltersLabel = new LabelImpl(text, LabelSmallFontSize)

    /**
      * Creates a normal label
      *
      * @param text the label text
      * @return a normal `FiltersLabel`
      */
    def normalLabel(text: String): FiltersLabel = new LabelImpl(text, LabelNormalFontSize)

    /**
      * Creates a big label
      *
      * @param text the label text
      * @return a big `FiltersLabel`
      */
    def bigLabel(text: String): FiltersLabel = new LabelImpl(text, LabelBigFontSize)

  }

  object FiltersVBoxes {

    /**
      * A custom `VBox` for filters' section
      */
    trait FiltersVBox extends VBox

    /**
      * A custom `VBox` for filters' section that contains a `RangeSlider` and some labels
      */
    trait SliderVBox extends FiltersVBox with DisablePane {

      /**
        *
        * @return the low value of the slider
        */
      def lowValue: Int

      /**
        *
        * @return the high value of the slider
        */
      def highValue: Int
    }

    private class DefaultVBox extends FiltersVBox

    private class BodyVBox extends FiltersVBox {
      margin = Insets(15, 0, 0, 0)
    }

    private class ComponentsVBox extends FiltersVBox {
      margin = Insets(10, 25, 0, 25)
    }

    private class BorderVBox extends FiltersVBox {
      border = new javafx.scene.layout.Border(new BorderStroke(Color.White,
        BorderStrokeStyle.Solid, CornerRadii.Empty, BorderWidths.Default))
    }

    private class SliderVBoxImpl(name: String) extends BodyVBox with SliderVBox {
      val label: FiltersLabel = normalLabel(name)
      val slider: FiltersLabeledSlider = labeledSlider
      children = label :: slider :: List()

      override def lowValue: Int = slider.lowValue()

      override def highValue: Int = slider.highValue()

      override def disableComponents(): Unit = slider.setDisable(true)

      override def enableComponents(): Unit = slider.setDisable(false)
    }


    /**
      * Creates a default VBox
      *
      * @return a `FiltersVBox`
      */
    def defaultVBox: FiltersVBox = new DefaultVBox

    /**
      * Creates a VBox for filters components
      *
      * @return a `FiltersVBox`
      */
    def componentsVBox: FiltersVBox = new ComponentsVBox

    /**
      * Creates a VBox with border
      *
      * @return a `FiltersVBox`
      */
    def borderVBox: FiltersVBox = new BorderVBox

    /**
      * Creates a VBox with left space
      *
      * @return a `FiltersVBox`
      */
    def bodyVBox: FiltersVBox = new BodyVBox

    /**
      * Creates a VBox with an embedded slider
      *
      * @param name the name of the slider
      * @return a `SliderVBox`
      */
    def sliderVBox(name: String): SliderVBox = new SliderVBoxImpl(name)

  }

  object FiltersHBoxes {

    val emptyItem: String = "No Selection"

    /**
      * A custom `HBox` for filters' section
      */
    trait FiltersHBox extends HBox

    /**
      * An HBox with an embedded `ChoiceBox`
      */
    trait ChoiceHBox extends FiltersHBox with DisablePane {

      /**
        *
        * @return the selected element of the `ChoiceBox`
        */
      def selectedItem: String

      /**
        * Sets the items of the `ChoiceBox`
        *
        * @param items the items of the choice box
        */
      def setItems(items: Seq[String])
    }

    private class DefaultHBox extends FiltersHBox

    private class ComponentsHBox extends FiltersHBox {
      margin = Insets(20, 0, 20, 0)
    }

    private class ChoiceHBoxImpl(name: String, items: Seq[String]) extends ComponentsHBox with ChoiceHBox {

      var previousItems: Set[String] = items.toSet
      val label: FiltersLabel = normalLabel(name)
      val choiceBox: FiltersChoiceBox[String] = defaultChoiceBox
      choiceBox.items = ObservableBuffer(emptyItem +: items)
      choiceBox.value = emptyItem
      children = label :: choiceBox :: List()

      override def disableComponents(): Unit = choiceBox.disable = true

      override def enableComponents(): Unit = choiceBox.disable = false

      override def selectedItem: String = choiceBox.value()

      override def setItems(items: Seq[String]): Unit = {
        if (items.toSet != previousItems) {
          previousItems = items.toSet
          choiceBox.items = ObservableBuffer(emptyItem +: items)
          choiceBox.value = emptyItem
        }
      }
    }

    /**
      * Creates a default HBox
      *
      * @return a `FiltersHBox`
      */
    def defaultHBox: FiltersHBox = new DefaultHBox

    /**
      * Creates an HBox for filters components
      *
      * @return a `FiltersHBox`
      */
    def componentsHBox: FiltersHBox = new ComponentsHBox

    /**
      * Creates an HBox with an embedded `ChoiceBox`
      *
      * @return a `FiltersHBox`
      */
    def choiceHBox(name: String, items: Seq[String]): ChoiceHBox = new ChoiceHBoxImpl(name, items)

  }

  object FiltersRadioButtons {

    /**
      * A custom `RadioButton` for filters' section
      */
    trait FiltersRadioButton extends RadioButton {
      textFill = backgroundColor
    }

    private class DefaultRadioButton(t: String) extends FiltersRadioButton {
      text = t
      margin = Insets(10, 0, 0, 0)
    }

    /**
      * Creates a default radio button
      *
      * @param text the text of the radio
      * @return a `FiltersRadioButton`
      */
    def defaultRadioButton(text: String): FiltersRadioButton = new DefaultRadioButton(text)

  }

  object FiltersSerparators {

    /**
      * A custom `Separator` for filters' section
      */
    trait FiltersSeparator extends Separator

    private class DefaultSeparator extends FiltersSeparator {
      margin = Insets(40, 0, 30, 0)
      style = s"-fx-background-color: White"
    }

    private class SmallSeparator extends FiltersSeparator {
      margin = Insets(0, 0, 5, 0)
    }

    /**
      * Creates a default separator
      *
      * @return a `FiltersSeparator`
      */
    def defaultSeparator: FiltersSeparator = new DefaultSeparator

    /**
      * Creates a small separator
      *
      * @return a `FiltersSeparator`
      */
    def smallSeparator: FiltersSeparator = new SmallSeparator

  }

  object FiltersChoiceBoxes {

    /**
      * A custom `ChoiceBox` for filters' section
      *
      * @tparam T the type of the ChoiceBox
      */
    trait FiltersChoiceBox[T] extends ChoiceBox[T]

    private class DefaultChoiceBox[T] extends FiltersChoiceBox[T] {
      margin = Insets(0, 0, 0, 50)
    }

    /**
      * Creates a default choice box
      *
      * @tparam T the type of the ChoiceBox
      * @return a `FiltersChoiceBox`
      */
    def defaultChoiceBox[T]: FiltersChoiceBox[T] = new DefaultChoiceBox[T]

  }

  object FiltersSliders {

    /**
      * A custom `RangeSlider` for filters' section
      */
    trait FiltersRangeSlider extends RangeSlider

    /**
      * An HBox with a range slider and labels embedded to observ values
      */
    trait FiltersLabeledSlider extends FiltersHBox with DisablePane {
      /**
        *
        * @return the low value of the slider
        */
      def lowValue: IntegerProperty

      /**
        *
        * @return the high value of the slider
        */
      def highValue: IntegerProperty
    }

    private class DefaultRangeSlider extends FiltersRangeSlider {
      setPadding(Insets(10, 0, 0, 50))
      setMin(MinSliderValue)
      setMax(MaxSliderValue)
      setLowValue(MinSliderValue)
      setHighValue(MaxSliderValue)
      setShowTickMarks(true)
      setShowTickLabels(true)
      setMajorTickUnit(25)
      setMinorTickCount(4)
      setBlockIncrement(1)

      prefWidthProperty().setValue(400)
    }

    private class LabeledSliderImpl extends FiltersLabeledSlider {
      val rangeSlider: FiltersRangeSlider = defaultRangeSlider
      val labelsVBox: FiltersVBox = defaultVBox
      val fromHBox: FiltersHBox = defaultHBox
      val toHBox: FiltersHBox = defaultHBox
      val fromLabel: FiltersLabel = smallLabel("From:")
      val toLabel: FiltersLabel = smallLabel("To:")
      val lowValueLabel: FiltersLabel = smallLabel(MinSliderValue.toString)
      val highValueLabel: FiltersLabel = smallLabel(MaxSliderValue.toString)

      fromHBox.children = fromLabel :: lowValueLabel :: List()
      toHBox.children = toLabel :: highValueLabel :: List()
      labelsVBox.children = fromHBox :: smallSeparator :: toHBox :: smallSeparator :: List()
      labelsVBox.margin = Insets(0, 0, 0, 40)
      fromLabel.prefWidth = 50
      toLabel.prefWidth = 50


      val lowValue: IntegerProperty = IntegerProperty(MinSliderValue)
      lowValue <== rangeSlider.lowValueProperty()
      val highValue: IntegerProperty = IntegerProperty(MaxSliderValue)
      highValue <== rangeSlider.highValueProperty()

      lowValueLabel.text <== lowValue.asString()
      highValueLabel.text <== highValue.asString()

      children += rangeSlider
      children += labelsVBox

      override def disableComponents(): Unit = ???

      override def enableComponents(): Unit = ???
    }

    /**
      * Creates a default range slider
      *
      * @return a `FiltersRangeSlider`
      */
    def defaultRangeSlider: FiltersRangeSlider = new DefaultRangeSlider

    /**
      * Creates a labeled range slider
      *
      * @return a `FiltersLabeledSlider`
      */
    def labeledSlider: FiltersLabeledSlider = new LabeledSliderImpl

  }

  object FiltersBorderPanes {

    /**
      * A custom `BorderPane` for filters' section
      */
    trait FiltersBorderPane extends BorderPane

    private class DefaultBorderPane extends FiltersBorderPane

    /**
      * Creates a default border pane
      *
      * @return a `FiltersBorderPane`
      */
    def defaultBorderPane: FiltersBorderPane = new DefaultBorderPane

  }



}


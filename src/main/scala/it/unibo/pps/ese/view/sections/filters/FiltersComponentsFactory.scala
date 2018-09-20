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


object FiltersComponentsFactory {

  private val backgroundColor: Color = Color.White

  private val LabelBigFontSize: Int = 30
  private val LabelNormalFontSize: Int = 22
  private val LabelSmallFontSize: Int = 15

  private val MinSliderValue = 0
  private val MaxSliderValue = 100


  object FiltersLabels {

    trait FiltersLabel extends Label {
      textFill = Color.White
    }

    private class LabelImpl(t: String, fontSize: Int) extends FiltersLabel {
      text = t
      style = s"-fx-font-size: $fontSize"
    }

    def smallLabel(text: String): FiltersLabel = new LabelImpl(text, LabelSmallFontSize)

    def normalLabel(text: String): FiltersLabel = new LabelImpl(text, LabelNormalFontSize)

    def bigLabel(text: String): FiltersLabel = new LabelImpl(text, LabelBigFontSize)


  }

  object FiltersVBoxes {


    trait FiltersVBox extends VBox

    trait SliderVBox extends FiltersVBox with DisablePane {
      def lowValue: Int
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

    def defaultVBox: FiltersVBox = new DefaultVBox

    def componentsVBox: FiltersVBox = new ComponentsVBox

    def borderVBox: FiltersVBox = new BorderVBox

    def bodyVBox: FiltersVBox = new BodyVBox

    def sliderVBox(name: String): SliderVBox = new SliderVBoxImpl(name)

  }

  object FiltersHBoxes {

    val emptyItem: String = "No Selection"

    trait FiltersHBox extends HBox

    trait ChoiceHBox extends FiltersHBox with DisablePane {
      def selectedItem: String
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


    def defaultHBox: FiltersHBox = new DefaultHBox

    def componentsHBox: FiltersHBox = new ComponentsHBox

    def choiceHBox(name: String, items: Seq[String]): ChoiceHBox = new ChoiceHBoxImpl(name, items)

  }

  object FiltersRadioButtons {

    trait FiltersRadioButton extends RadioButton {
      textFill = backgroundColor
    }

    private class DefaultRadioButton(t: String) extends FiltersRadioButton {
      text = t
      margin = Insets(10, 0, 0, 0)
    }

    def defaultRadioButton(text: String): FiltersRadioButton = new DefaultRadioButton(text)

  }

  object FiltersSerparators {

    trait FiltersSeparator extends Separator

    private class DefaultSeparator extends FiltersSeparator {
      margin = Insets(40, 0, 30, 0)
      style = s"-fx-background-color: White"
    }

    private class SmallSeparator extends FiltersSeparator {
      margin = Insets(0, 0, 5, 0)
    }

    def defaultSeparator: FiltersSeparator = new DefaultSeparator

    def smallSeparator: FiltersSeparator = new SmallSeparator

  }

  object FiltersChoiceBoxes {

    trait FiltersChoiceBox[T] extends ChoiceBox[T]

    private class DefaultChoiceBox[T] extends FiltersChoiceBox[T] {
      margin = Insets(0, 0, 0, 50)
    }

    def defaultChoiceBox[T]: FiltersChoiceBox[T] = new DefaultChoiceBox[T]

  }

  object FiltersSliders {


    trait FiltersRangeSlider extends RangeSlider

    trait FiltersLabeledSlider extends FiltersHBox with DisablePane {
      def lowValue: IntegerProperty
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

    def defaultRangeSlider: FiltersRangeSlider = new DefaultRangeSlider

    def labeledSlider: FiltersLabeledSlider = new LabeledSliderImpl

  }

  object FiltersBorderPanes {

    trait FiltersBorderPane extends BorderPane

    private class DefaultBorderPane extends FiltersBorderPane

    def defaultBorderPane: FiltersBorderPane = new DefaultBorderPane

  }



}


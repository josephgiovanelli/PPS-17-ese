package it.unibo.pps.ese.view.filters

import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersLabels.FiltersLabel
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersSliders._
import it.unibo.pps.ese.view.filters.FiltersComponentsFactory.FiltersLabels._
import org.controlsfx.control.RangeSlider
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.paint.Color


object FiltersComponentsFactory {

  private val backgroundColor: Color = Color.White

  private val LabelBigFontSize: Int = 40
  private val LabelNormalFontSize: Int = 25
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

    private class DefaultVBox extends FiltersVBox


    private class ComponentsVBox extends FiltersVBox {
      margin = Insets(10, 25, 0, 25)
    }

    private class BorderVBox extends FiltersVBox {
      border = new javafx.scene.layout.Border(new BorderStroke(Color.White,
        BorderStrokeStyle.Solid, CornerRadii.Empty, BorderWidths.Default))
    }

    def defaultVBox: FiltersVBox = new DefaultVBox

    def componentsVBox: FiltersVBox = new ComponentsVBox

    def borderVBox: FiltersVBox = new BorderVBox

  }

  object FiltersHBoxes {

    trait FiltersHBox extends HBox

    private class DefaultHBox extends FiltersHBox

    private class ComponentsHBox extends FiltersHBox {
      margin = Insets(20, 0, 30, 0)
    }

    private class HBoxWithLabeledSlider extends FiltersHBox {
      val rangeSlider: FiltersRangeSlider = defaultRangeSlider
      val lowValue: FiltersLabel = smallLabel(MinSliderValue.toString)
      val highValue: FiltersLabel = smallLabel(MaxSliderValue.toString)


    }

    def defaultHBox: FiltersHBox = new DefaultHBox

    def componentsHBox: FiltersHBox = new ComponentsHBox

    def hBoxWithLabeledSlider: FiltersHBox = new HBoxWithLabeledSlider

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
      margin = Insets(40, 0, 40, 0)
      style = s"-fx-background-color: White"
    }

    def defaultSeparator: FiltersSeparator = new DefaultSeparator

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

      prefWidthProperty().setValue(400)
    }

    def defaultRangeSlider: FiltersRangeSlider = new DefaultRangeSlider

  }



}


package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog

import it.unibo.pps.ese.view.configuration.dialogs.{AbstractDialog, BackPane, MainDialog}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, Pane, VBox}
import scalafx.stage.Window

case class ConversionMapPane(mainDialog: MainDialog,
                             override val previousContent: Option[Pane],
                             currentConversion: Option[(String, Double)],
                             qualities: Set[String]) extends BackPane[(String, Double)](mainDialog, previousContent, None) {

  /*
  Header
  */

  mainDialog.title = "Conversion Map Dialog"
  mainDialog.headerText = "Define a conversion map"

  /*
  Fields
   */

  val conversionName = new ComboBox(ObservableBuffer[String](qualities.toSeq))
  val previousConversionName = new TextField()

  val conversionValue: TextField = new TextField()

  fields = ListMap(
    conversionValue -> (new Label("Value"), new Label("")),
  )

  val grid: GridPane = createGrid(1)
  grid.vgap = 10
  grid.add(new Label("Name"), 0, 0)
  grid.add(if (currentConversion.isDefined) previousConversionName else conversionName, 1, 0)

  center = new VBox() {
    children ++= Seq(grid)
    styleClass += "sample-page"
  }


  /*
  Checks
   */

  mandatoryFields = fields.keySet
  doubleFields = fields.keySet

  createChecks()

  /*
  Restart information
  */

  if (currentConversion.isDefined) {
    conversionName.value.value = currentConversion.get._1
    previousConversionName.editable = false
    previousConversionName.text.value = currentConversion.get._1
    conversionValue.text.value = currentConversion.get._2.toString
    okButton.disable = false
  } else {
    conversionName.value.value = qualities.head
  }

  /*
  Result
  */

//  resultConverter = dialogButton =>
//    if (dialogButton == okButtonType) (conversionName.value.value, conversionValue.text.value.toDouble)
//    else null

}




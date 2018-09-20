package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs


import it.unibo.pps.ese.view.configuration.dialogs.components.{ErrorLabel, WhiteLabel}
import it.unibo.pps.ese.view.configuration.dialogs.{AbstractDialog, BackPane, MainDialog}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.control._
import scalafx.scene.layout.Pane
import scalafx.stage.Window

case class EffectPane(mainDialog: MainDialog,
                      override val previousContent: Option[AllelePane],
                      currentEffect: (String, Double))
  extends BackPane[(String, Double)](mainDialog, previousContent, None, "", "", "") {

  /*
  Header
   */

//  title = "Effect Dialog"
//  headerText = "Define an allele effect"

  /*
  Fields
   */


  val propertyName: TextField = new TextField()
  val effectValue: TextField = new TextField()

  fields = ListMap(
    propertyName -> (new WhiteLabel("Name"), new ErrorLabel("")),
    effectValue -> (new WhiteLabel("Value"), new ErrorLabel(""))
  )

  center = createGrid(0)

  Platform.runLater(propertyName.requestFocus())

  /*
  Checks
   */

  mandatoryFields = fields.keySet
  doubleFields = mandatoryFields - propertyName

  createChecks()


  /*
  Restart information
   */

  propertyName.editable = false
  propertyName.text.value = currentEffect._1
  effectValue.text.value = currentEffect._2.toString

  /*
  Result
   */


  okButton.onAction = _ => {
    previousContent.get.confirmAddEffect(propertyName.text.value, effectValue.text.value.toDouble)
  }
}





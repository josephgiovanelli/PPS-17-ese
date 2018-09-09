package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs


import it.unibo.pps.ese.view.configuration.dialogs.AbstractDialog

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.control._
import scalafx.stage.Window

case class EffectDialog(window: Window, currentEffect: (String, Double)) extends AbstractDialog[(String, Double)](window, None) {

  /*
  Header
   */

  title = "Effect Dialog"
  headerText = "Define an allele effect"

  /*
  Fields
   */


  val propertyName: TextField = new TextField()
  val effectValue: TextField = new TextField()

  fields = ListMap(
    propertyName -> (new Label("Name"), new Label("")),
    effectValue -> (new Label("Value"), new Label(""))
  )

  dialogPane().content = createGrid(0)

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

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) (propertyName.text.value, effectValue.text.value.toDouble)
    else null
}





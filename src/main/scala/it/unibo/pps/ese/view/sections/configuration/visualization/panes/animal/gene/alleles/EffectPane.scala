package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.alleles


import it.unibo.pps.ese.view.sections.configuration.visualization.core.{BackPane, MainDialog}
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{ErrorLabel, WhiteLabel}

import scala.collection.immutable.ListMap
import scalafx.application.Platform
import scalafx.scene.control._

object EffectProperties {
    val title = "Effect Pane"
    val headerText = "Define an allele effect"
}

import EffectProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

case class EffectPane(mainDialog: MainDialog,
                      override val previousContent: Option[AllelePane],
                      currentEffect: (String, Double))
  extends BackPane[(String, Double)](mainDialog, previousContent, None, title, headerText, previousContent.get.path + newLine(6) + title) {

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





package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.alleles


import it.unibo.pps.ese.view.sections.configuration.visualization.core.{AbstractPane, MainDialog}
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{ErrorLabel, WhiteLabel}

import scala.collection.immutable.ListMap
import scalafx.application.Platform
import scalafx.scene.control._

/**
  * It defines the title and the header
  */
object EffectProperties {
    val title = "Effect Pane"
    val headerText = "Define an allele effect"
}

import EffectProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

/**
  * The pane that allows to insert the relation between the allele and a property.
  *
  * @param mainDialog the main dialog with which communicating
  * @param previousContent the previous content
  * @param currentEffect the previous effect if is present
  */
case class EffectPane(mainDialog: MainDialog,
                      override val previousContent: Option[AllelePane],
                      currentEffect: (String, Double))
  extends AbstractPane[(String, Double)](mainDialog, previousContent, None, title, headerText, previousContent.get.path + newLine(6) + title, 6) {

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





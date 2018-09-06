package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs

import javafx.scene.Node

import it.unibo.pps.ese.view.configuration.dialogs._

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.css.PseudoClass
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Window

case class AlleleDialog(window: Window, animal: String, gene: String, currentIdAllele: Option[String], properties: Set[String], chromosomeTypes: ChromosomeTypes.Value) extends Dialog[AlleleInfo] {

  /*
  Header
   */

  initOwner(window)
  title = "Allele Dialog"
  headerText = "Create an allele"
  dialogPane().getStylesheets.add(getClass.getResource("/red-border.css").toExternalForm)
  val errorClass = PseudoClass("error")

  /*
  Fields
  */


  val idAllele: TextField = new TextField()
  val dominance: TextField = new TextField()
  val consume: TextField = new TextField()
  val probability: TextField = new TextField()

  val fields: Map[TextField, (Label, Label)] = ListMap(
    idAllele -> (new Label("Id"), new Label("")),
    dominance -> (new Label("Dominance"), new Label("")),
    consume -> (new Label("Consume"), new Label("")),
    probability -> (new Label("Probability"), new Label("")),
  )


  val grid: GridPane = new GridPane() {
    hgap = 10
    padding = Insets(20, 100, 10, 10)

    var count = 0
    fields.foreach(field => {
      add(field._2._1, 0, count)
      add(field._1, 1, count)
      count += 1
      add(field._2._2, 1, count)
      count += 1
      field._2._2.textFill = Color.Red
    })
  }

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalInfo(animal) match {
    case Some((_, chromosomeInfo)) => chromosomeInfo
    case None => throw new IllegalStateException()
  }

  val currentSpecificAnimalChromosome = chromosomeTypes match {
    case ChromosomeTypes.STRUCTURAL => currentAnimalChromosome.structuralChromosome
    case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome
    case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome
  }


  var currentAlleles: Map[String, AlleleInfo] = currentSpecificAnimalChromosome.get(gene) match {
    case Some((_, alleles)) => alleles
    case None => throw new IllegalStateException()
  }

  var effects:  Map[String, Double] =
    if (currentIdAllele.isDefined) currentAlleles(currentIdAllele.get).effect
    else properties.map(x => (x, 0.0)).groupBy(_._1).map{ case (k,v) => (k,v.map(_._2))}.map(x => x._1 -> x._2.head)
  val effectsName: ObservableBuffer[String] = ObservableBuffer[String](effects.keySet toSeq)
  val effectsListView: ListView[String] = new ListView[String] {
    items = effectsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        EffectDialog(window, (value, effects(value))).showAndWait() match {
          case Some((name: String, value: Double)) =>
            effects += (name -> value)
          case None => println("Dialog returned: None")
        }
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

  effectsListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT

  dialogPane().content = new VBox() {
    children ++= Seq(grid, new Label("Effects"), effectsListView, new Label("At least one effect"))
    styleClass += "sample-page"
  }

  Platform.runLater(idAllele.requestFocus())

  /*
  OkButton
  */

  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = true

  /*
  Checks
  */
  val mandatoryFields: Set[TextField] = fields.keySet
  val doubleFields: Set[TextField] = mandatoryFields - idAllele
  val specialFields: Set[TextField] = Set(probability)
  val allelesId: Set[String] = (currentAnimalChromosome.structuralChromosome ++
    currentAnimalChromosome.regulationChromosome ++
    currentAnimalChromosome.sexualChromosome).values.flatMap(x => x._2.keySet) toSet


  mandatoryFields.foreach(subject => {
    subject.text.onChange ( (_, _, newValue) => {
      okButton.disable = checkFields(subject, newValue)
    })
  })

  effectsName.onChange((_,_) =>
    okButton.disable = checkFields)

  /*
  Restart information
  */

  if (currentIdAllele.isDefined) {
    idAllele.editable = false
    idAllele.text.value = currentAlleles(currentIdAllele.get).id
    dominance.text.value = currentAlleles(currentIdAllele.get).dominance.toString
    consume.text.value = currentAlleles(currentIdAllele.get).consume.toString
    probability.text.value = currentAlleles(currentIdAllele.get).probability.toString
  }

  /*
  Result
   */

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType)
      AlleleInfo(gene, idAllele.text.value, dominance.text.value.toDouble, consume.text.value.toDouble, probability.text.value.toDouble, effects)
    else
      null

  private def checkFields(field: TextField, newValue: String): Boolean = {
    val mandatoryCheck = field.getText.trim().isEmpty
    val doubleCheck = if (doubleFields.contains(field)) ParseUtils.parse[Double](field.getText.trim()).isEmpty else false
    val specialCheck =
      if (specialFields.contains(field))
        if (ParseUtils.parse[Double](field.getText.trim()).isEmpty)
          true
        else
          field.text.value.toDouble < 0.0 || field.text.value.toDouble > 1.0
      else
        false
    val uniqueIdCheck = if (field.equals(idAllele) && currentIdAllele.isEmpty) allelesId.contains(idAllele.text.value) else false
    val lengthCheck = if (field.equals(idAllele)) idAllele.text.value.length != EntitiesInfo.instance().getAnimalInfo(animal).get._1.alleleLength
                      else false

    if (mandatoryCheck || lengthCheck || doubleCheck || specialCheck || uniqueIdCheck)
      field.pseudoClassStateChanged(errorClass, true)
    else
      field.pseudoClassStateChanged(errorClass, false)

    if (mandatoryCheck) fields(field)._2.text.value = "Must be filled"
    else if (lengthCheck) fields(field)._2.text.value = "Must be " + EntitiesInfo.instance().getAnimalInfo(animal).get._1.alleleLength + " long"
    else if (doubleCheck) fields(field)._2.text.value = "Must be double"
    else if (specialCheck) fields(field)._2.text.value = "Must be a probability"
    else if (uniqueIdCheck) fields(field)._2.text.value = "Must be unique"
    else fields(field)._2.text.value = ""

    checkFields
  }

  private def checkFields: Boolean = mandatoryFields.exists(x => x.getText.trim().isEmpty) ||
    idAllele.text.value.length != EntitiesInfo.instance().getAnimalInfo(animal).get._1.alleleLength ||
    doubleFields.exists(x => ParseUtils.parse[Double](x.getText.trim()).isEmpty) ||
    specialFields.exists(x => x.text.value.toDouble < 0.0 || x.text.value.toDouble > 1.0) ||
    (allelesId.contains(idAllele.text.value) && currentIdAllele.isEmpty) ||
    effectsName.isEmpty


}


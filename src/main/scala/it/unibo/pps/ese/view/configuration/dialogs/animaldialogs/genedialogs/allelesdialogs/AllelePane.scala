package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs


import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.entitiesinfo._
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals.{AlleleInfo, AnimalChromosomeInfo, ChromosomeInfo}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, Pane, VBox}
import scalafx.stage.Window

case class AllelePane(mainDialog: MainDialog,
                      override val previousContent: Option[AllelesPane],
                      animal: String,
                      gene: String,
                      allele: Option[String],
                      properties: Set[String],
                      chromosomeTypes: ChromosomeTypes.Value) extends BackPane[AlleleInfo](mainDialog, previousContent, allele) {

  /*
  Header
   */

  mainDialog.title = "Allele Dialog"
  mainDialog.headerText = "Create an allele"

  /*
  Fields
  */


  val idAllele: TextField = new TextField()
  val dominance: TextField = new TextField()
  val consume: TextField = new TextField()
  val probability: TextField = new TextField()

  fields = ListMap(
    idAllele -> (new Label("Id"), new Label("")),
    dominance -> (new Label("Dominance"), new Label("")),
    consume -> (new Label("Consume"), new Label("")),
    probability -> (new Label("Probability"), new Label("")),
  )


  val grid: GridPane = createGrid(0)

  val currentAnimalChromosome: AnimalChromosomeInfo = EntitiesInfo.instance().getAnimalChromosomeInfo(animal)


  val currentSpecificAnimalChromosome: Map[String, ChromosomeInfo] = chromosomeTypes match {
    case ChromosomeTypes.STRUCTURAL => currentAnimalChromosome.structuralChromosome
    case ChromosomeTypes.REGULATION => currentAnimalChromosome.regulationChromosome
    case ChromosomeTypes.SEXUAL => currentAnimalChromosome.sexualChromosome
  }


  var currentAlleles: Map[String, AlleleInfo] = currentSpecificAnimalChromosome.get(gene) match {
    case Some(chromosomeInfo) => chromosomeInfo.alleles
    case None => throw new IllegalStateException()
  }

  var effects:  Map[String, Double] =
    if (allele.isDefined) currentAlleles(allele.get).effect
    else properties.map(x => (x, 0.0)).groupBy(_._1).map{ case (k,v) => (k,v.map(_._2))}.map(x => x._1 -> x._2.head)
  val effectsName: ObservableBuffer[String] = ObservableBuffer[String](effects.keySet toSeq)
  val effectsListView: ListView[String] = new ListView[String] {
    items = effectsName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        mainDialog.setContent(EffectPane(mainDialog, Some(AllelePane.this), (value, effects(value))))
//        fatto
//          .showAndWait() match {
//          case Some((name: String, value: Double)) =>
//            effects += (name -> value)
//          case None => println("Dialog returned: None")
//        }
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }

//  effectsListView.prefHeight = MIN_ELEM * ROW_HEIGHT

  center = new VBox() {
    children ++= Seq(grid, new Label("Effects"), effectsListView, new Label("At least one effect"))
    styleClass += "sample-page"
  }

  Platform.runLater(idAllele.requestFocus())

  /*
  Checks
  */

  val allelesId: Set[String] = (currentAnimalChromosome.structuralChromosome ++
    currentAnimalChromosome.regulationChromosome ++
    currentAnimalChromosome.sexualChromosome).values.flatMap(x => x.alleles.keySet) toSet

  mandatoryFields = fields.keySet
  doubleFields = mandatoryFields - idAllele
  listFields = Seq(effectsName)
  uniqueFields = Map(idAllele -> allelesId)
  lengthFields = Map(idAllele -> EntitiesInfo.instance().getAnimalBaseInfo(animal).alleleLength)
  probabilityFields = Set(probability)

  createChecks()


  /*
  Restart information
  */

  if (allele.isDefined) {
    idAllele.editable = false
    idAllele.text.value = currentAlleles(allele.get).id
    dominance.text.value = currentAlleles(allele.get).dominance.toString
    consume.text.value = currentAlleles(allele.get).consume.toString
    probability.text.value = currentAlleles(allele.get).probability.toString
  }

  /*
  Result
   */

  okButton.onAction = _ => {
    allele match {
      case Some(_) => previousContent.get.confirmModifyAlleleInfo(AlleleInfo(gene, idAllele.text.value, dominance.text.value.toDouble,
        consume.text.value.toDouble, probability.text.value.toDouble, effects))
      case None => previousContent.get.confirmAddAlleleInfo(AlleleInfo(gene, idAllele.text.value, dominance.text.value.toDouble,
        consume.text.value.toDouble, probability.text.value.toDouble, effects))
    }
  }

  def confirmAddEffect(name: String, value: Double): Unit = {
    effects += (name -> value)
    mainDialog.setContent(this)
  }
}


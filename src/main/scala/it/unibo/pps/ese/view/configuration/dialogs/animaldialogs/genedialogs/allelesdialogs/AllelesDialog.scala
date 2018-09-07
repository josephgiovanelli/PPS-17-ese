package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.allelesdialogs

import it.unibo.pps.ese.view.configuration.dialogs._
import it.unibo.pps.ese.view.configuration.entitiesinfo._
import it.unibo.pps.ese.view.configuration.entitiesinfo.support.animals.{AlleleInfo, AnimalChromosomeInfo, ChromosomeInfo, GeneInfo}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.Window

case class AllelesDialog(window: Window, animal: String, gene: String, chromosomeTypes: ChromosomeTypes.Value) extends AbstractDialog(window, None) {

  /*
  Header
  */

  title = "Alleles Dialog"
  headerText = "Define chromosome alleles"

  /*
  Fields
  */

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

  var properties: Set[String] = currentSpecificAnimalChromosome(gene).geneInfo.properties.keySet
  val allelesName: ObservableBuffer[String] = ObservableBuffer[String](currentAlleles.keySet toSeq)
  val allelesListView: ListView[String] = new ListView[String] {
    items = allelesName
    selectionModel().selectedItem.onChange( (_, _, value) => {
      if (selectionModel().getSelectedIndex != -1) {
        val missedProperties: Map[String, Double] = (properties -- currentAlleles(value).effect.keySet).map(x => (x, 0.0)).groupBy(_._1).map{ case (k,v) => (k,v.map(_._2))}.map(x => x._1 -> x._2.head)
        val currentAllele: AlleleInfo = currentAlleles(value)
        currentAllele.effect ++= missedProperties
        currentAlleles += (value -> currentAllele)
        EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
        AlleleDialog(window, animal, gene, Some(value), properties, chromosomeTypes).showAndWait() match {
          case Some(AlleleInfo(alleleGene, id, dominance, consume, probability, effect)) =>
            currentAlleles += (id -> AlleleInfo(alleleGene, id, dominance, consume, probability, effect))
            EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
          case None => println("Dialog returned: None")
        }
        Platform.runLater(selectionModel().clearSelection())
      }
    })
  }



  allelesListView.prefHeight = ListViewUtils.MIN_ELEM * ListViewUtils.ROW_HEIGHT

  val allelesButton = new Button("Add")
  allelesButton.onAction = _ => AlleleDialog(window, animal, gene, None, properties, chromosomeTypes).showAndWait() match {
    case Some(AlleleInfo(alleleGene, id, dominance, consume, probability, effect)) =>
      currentAlleles += (id -> AlleleInfo(alleleGene, id, dominance, consume, probability, effect))
      allelesName.insert(allelesName.size, id)
      EntitiesInfo.instance().setChromosomeAlleles(animal, chromosomeTypes, gene, currentAlleles)
    case None => println("Dialog returned: None")
  }

  val allelesPane = new BorderPane()
  allelesPane.left = new Label("Alleles")
  allelesPane.right = allelesButton

  dialogPane().content = new VBox() {
    children ++= Seq(allelesPane, allelesListView, new Label("At least one allele"))
    styleClass += "sample-page"
  }

  /*
  Checks
   */

  listFields = Seq(allelesName)
  createChecks()

}


package it.unibo.pps.ese.model.genetics.dnaexpression

import it.unibo.pps.ese.model.genetics.dna.{GeneWithAllelicForms, MGene}
import it.unibo.pps.ese.model.genetics.entities.QualityType

/**
  * Aggregated information about Gene
  */
sealed trait GeneStats
case class AllelicGeneStats(
                             gene:GeneWithAllelicForms,
                             dominanceLevel:Double,
                             probability:Double,
                             active:Boolean,
                             affectedQualities:Seq[QualityType],
                             features:Seq[(String,Double)]
                           ) extends GeneStats
case class BasicGeneStats(
                           gene:MGene,
                           identifiedThing:String
                         ) extends GeneStats

case class EmptyGeneStats() extends GeneStats

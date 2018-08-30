package it.unibo.pps.ese.genetics.dnaexpression

import it.unibo.pps.ese.genetics.dna.{GeneWithAllelicForms, MGene}
import it.unibo.pps.ese.genetics.entities.QualityType

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


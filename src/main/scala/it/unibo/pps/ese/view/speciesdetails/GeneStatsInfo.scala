package it.unibo.pps.ese.view.speciesdetails

import it.unibo.pps.ese.genetics.GeneticsSimulator
import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.dna.MGene
import it.unibo.pps.ese.genetics.dnaexpression.GeneStats
import it.unibo.pps.ese.genetics.entities.AnimalInfo
private object StatsUtilities{
  implicit class RichGeneticsSimulator(geneticsSimulator: GeneticsSimulator){
    implicit def coupledGeneStats(genes:(MGene,MGene),animalInfo: AnimalInfo):GeneCouple = {
      GeneCouple(geneticsSimulator.getGeneStats(genes._1,animalInfo),
                  geneticsSimulator.getGeneStats(genes._2,animalInfo))
    }
  }
}

case class GeneCouple(gene1: GeneStats, gene2: GeneStats)
case class ChromosomeWithGeneCouple(chromosomeType: ChromosomeType,geneCouple:GeneCouple)
object GenomeStatsUtilities{
  import StatsUtilities._
  def buildGenomeStats(geneticsSimulator: GeneticsSimulator,animalInfo: AnimalInfo):List[ChromosomeWithGeneCouple] = {
    animalInfo.genome.coupledGene.flatMap({case(k,v)=>
      v.map(c=>ChromosomeWithGeneCouple(k,geneticsSimulator.coupledGeneStats(c,animalInfo)))
    }).toList
  }
}

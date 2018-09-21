package it.unibo.pps.ese.view.sections.speciesdetails

import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.model.genetics.dna.{GeneInChromosome, MGene}
import it.unibo.pps.ese.model.genetics.dnaexpression.{EmptyGeneStats, GeneStats}
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo
private object StatsUtilities{
  implicit class RichGeneticsSimulator(geneticsSimulator: GeneticsSimulator){
    implicit def coupledGeneStats(chromosomeType: ChromosomeType,genes:(MGene,MGene),animalInfo: AnimalInfo):GeneCouple = {
      GeneCouple(
                  GeneStatsInChromosome(chromosomeType,geneticsSimulator.getGeneStats(genes._1,animalInfo)),
                  GeneStatsInChromosome(chromosomeType,geneticsSimulator.getGeneStats(genes._2,animalInfo))
      )
    }

    implicit def sexualGeneCoupledStats(genes:(GeneInChromosome,GeneInChromosome),animalInfo: AnimalInfo):GeneCouple = {
      def getGeneStats(gene:GeneInChromosome):GeneStatsInChromosome= gene.gene match {
        case Some(g)=> GeneStatsInChromosome(
          gene.chromosomeType,
          geneticsSimulator.getGeneStats(g,animalInfo)
        )
        case None => GeneStatsInChromosome(
          gene.chromosomeType,
          EmptyGeneStats()
        )
      }
      GeneCouple(getGeneStats(genes._1),getGeneStats(genes._2))
    }
  }
}

case class GeneStatsInChromosome(chromosomeType: ChromosomeType,geneStats: GeneStats)
case class GeneCouple(gene1: GeneStatsInChromosome,gene2: GeneStatsInChromosome)
object GenomeStatsUtilities{
  import StatsUtilities._
  def buildGenomeStats(geneticsSimulator: GeneticsSimulator,animalInfo: AnimalInfo):List[GeneCouple] = {
    animalInfo.genome.coupledGene.flatMap({case(k,v)=>
      v.map(c=>geneticsSimulator.coupledGeneStats(k,c,animalInfo))
    }).toList ++
    animalInfo
      .genome
      .sexualGeneCoupled
      .map(geneticsSimulator.sexualGeneCoupledStats(_,animalInfo))
  }
}

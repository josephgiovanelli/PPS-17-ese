package it.unibo.pps.ese.view.sections.genome

import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.model.genetics.dna.{GeneInChromosome, MGene}
import it.unibo.pps.ese.model.genetics.dnaexpression.{EmptyGeneStats, GeneStats}
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo


private object StatsUtilities{

  /**
    * A implicit class to "pimp" [[GeneticsSimulator]] with methods to retrieve information about certain genes
    * @param geneticsSimulator
    */
  implicit class RichGeneticsSimulator(geneticsSimulator: GeneticsSimulator){
    /**
      * Method to obtain the coupled gene information given two gene
      * @param chromosomeType
      *                       The chromosome to which the gene belongs
      * @param genes
      *              The genes
      * @param animalInfo
      *                  The [[AnimalInfo]] of the selected animal
      * @return
      *         The [[GeneInformationCoupled]] of the two genes from the selected animal
      */
    def coupledGeneStats(chromosomeType: ChromosomeType,genes:(MGene,MGene),animalInfo: AnimalInfo):GeneInformationCoupled = {
      GeneInformationCoupled(
                  GeneStatsInChromosome(chromosomeType,geneticsSimulator.getGeneStats(genes._1,animalInfo)),
                  GeneStatsInChromosome(chromosomeType,geneticsSimulator.getGeneStats(genes._2,animalInfo))
      )
    }

    /**
      * Method to obtain the [[GeneInformationCoupled]] about the given sexual genes couple.
      * A different method is given because in the X chromosome there can be genes thar aren't in the Y chromosome
      *
      * @param genes
      *                    The [[GeneInChromosome]] couple,
      * @param animalInfo
      * *                  The [[AnimalInfo]] of the selected animal
      * @return
      * *         The [[GeneInformationCoupled]] of the two genes from the selected animal
      */
     def sexualGeneCoupledStats(genes:(GeneInChromosome,GeneInChromosome),animalInfo: AnimalInfo):GeneInformationCoupled = {
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
      GeneInformationCoupled(getGeneStats(genes._1),getGeneStats(genes._2))
    }
  }
}

case class GeneStatsInChromosome(chromosomeType: ChromosomeType,geneStats: GeneStats)
case class GeneInformationCoupled(gene1: GeneStatsInChromosome, gene2: GeneStatsInChromosome)
object GenomeStatsUtilities{
  import StatsUtilities._
  def buildGenomeStats(geneticsSimulator: GeneticsSimulator,animalInfo: AnimalInfo):List[GeneInformationCoupled] = {
    animalInfo.genome.coupledGene.flatMap{case(k,v)=>
      v.map(c=>geneticsSimulator.coupledGeneStats(k,c,animalInfo))
    }.toList ++
    animalInfo
      .genome
      .sexualGeneCoupled
      .map(geneticsSimulator.sexualGeneCoupledStats(_,animalInfo))
  }
}

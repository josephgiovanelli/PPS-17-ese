package it.unibo.pps.ese.model.genetics.generators

import it.unibo.pps.ese.model.genetics.Utilities
import it.unibo.pps.ese.model.genetics.dna._
import it.unibo.pps.ese.model.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.model.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.model.genetics.entities.{Female, Male}

import scala.annotation.tailrec

case class GeneWithPossibleAlleles(baseGeneSeq:Seq[ProteinoGenicAmminoacid],alleles:Seq[AlleleWithProbability])
case class AlleleWithProbability(allelicSeq:Seq[ProteinoGenicAmminoacid],probability:Double)

sealed trait AnimalGenomeSupplier{
  def generateAnimalGenome:AnimalGenome
}
class SpeciesGenerator(
                        commonChromosomeGenes:Seq[BasicGene],
                        structuralChromosomeGenes:Seq[GeneWithPossibleAlleles],
                        lifeCycleChromosomeGenes:Seq[GeneWithPossibleAlleles],
                        feedingChromosomeGenes:Seq[MGene],
                        sexualChromosomeGenes:Seq[GeneWithPossibleAlleles]
) extends AnimalGenomeSupplier {
  override def generateAnimalGenome: AnimalGenome = {
    import ChromosomeBuildingUtilities._
    val ccc:ChromosomeCouple = buildCommonChromosomeCouple
    val scc:ChromosomeCouple = buildStructuralChromosomeCouple
    val lfcc:ChromosomeCouple = buildLifeCycleChromosomeCouple
    val fcc:ChromosomeCouple = buildFeedingChromosomeCouple
    val sxcc:SexualChromosomeCouple = buildSexualChromosomeCouple
    AnimalGenome(
      Map(
        ChromosomeType.COMMON -> ccc,
        ChromosomeType.STRUCTURAL_ANIMAL ->scc,
        ChromosomeType.LIFE_CYCLE -> lfcc,
        ChromosomeType.FEEDING-> fcc
      ),
      sxcc
    )
  }

  private[this] object ChromosomeBuildingUtilities{
    def buildCommonChromosomeCouple:ChromosomeCouple = {
      buildChromosomeCouple(commonChromosomeGenes,commonChromosomeGenes,ChromosomeType.COMMON)
    }

    @tailrec
    def _createGeneCouples(
                                    gt:GeneType,
                                    gs:Seq[GeneWithPossibleAlleles],
                                    gl:Seq[(GeneWithAllelicForms)])
    :Seq[(MGene)] = gs match {
      case h+:t =>
        val map:Map[Seq[ProteinoGenicAmminoacid],Double] = h.alleles.map(a=>(a.allelicSeq,a.probability)).toMap
        val g1:GeneWithAllelicForms = GeneWithAllelicForms(h.baseGeneSeq,Utilities.sample(map),gt)
        _createGeneCouples(gt,t,gl :+ g1)
      case _ =>gl
    }

    def buildChromosomeCouple(
                                       gs1:Seq[MGene],
                                       gs2:Seq[MGene],
                                       chromosomeType: ChromosomeType):ChromosomeCouple = {
      val c1:Chromosome = Chromosome(chromosomeType,gs1: _*)
      val c2:Chromosome = Chromosome(chromosomeType,gs2: _*)
      ChromosomeCouple(c1,c2)
    }

    def buildStructuralChromosomeCouple:ChromosomeCouple = {
      val sgs1:Seq[MGene] = _createGeneCouples(StructuralGene,structuralChromosomeGenes,List())
      val sgs2:Seq[MGene] = _createGeneCouples(StructuralGene,structuralChromosomeGenes,List())
      buildChromosomeCouple(sgs1,sgs2,ChromosomeType.STRUCTURAL_ANIMAL)
    }

    def buildLifeCycleChromosomeCouple:ChromosomeCouple = {
      val lcgs1:Seq[MGene] = _createGeneCouples(RegulatorGene,lifeCycleChromosomeGenes,List())
      val lcgs2:Seq[MGene] = _createGeneCouples(RegulatorGene,lifeCycleChromosomeGenes,List())
      buildChromosomeCouple(lcgs1,lcgs2,ChromosomeType.LIFE_CYCLE)
    }

    def buildFeedingChromosomeCouple:ChromosomeCouple = {
      buildChromosomeCouple(feedingChromosomeGenes,feedingChromosomeGenes,ChromosomeType.FEEDING)
    }

    def buildSexualChromosomeCouple:SexualChromosomeCouple = {
      val sgs1:Seq[MGene] = _createGeneCouples(RegulatorGene,sexualChromosomeGenes,List())
      val sgs2:Seq[MGene] = _createGeneCouples(RegulatorGene,sexualChromosomeGenes,List())
      def _secondChromosome:SexualChromosome = Utilities.pickRandomElement(Male, Female) match {
        case Male =>
          Chromosome(ChromosomeType.SEXUAL_Y, Y)
        case Female =>
          Chromosome(ChromosomeType.SEXUAL_X, X, sgs2: _*)
      }
      val cs1:SexualChromosome = Chromosome(ChromosomeType.SEXUAL_X, X, sgs1: _*)
      val cs2:SexualChromosome = _secondChromosome
      SexualChromosomeCouple(cs1,cs2)
    }
  }

}

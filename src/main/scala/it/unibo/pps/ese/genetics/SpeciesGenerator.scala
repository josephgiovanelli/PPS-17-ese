package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.genetics.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid

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

  private def buildCommonChromosomeCouple:ChromosomeCouple = {
    buildChromosomeCouple(commonChromosomeGenes,commonChromosomeGenes,ChromosomeType.COMMON)
  }

  @tailrec
  private def _createGeneCouples(
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

  private def buildChromosomeCouple(
                                     gs1:Seq[MGene],
                                     gs2:Seq[MGene],
                                     chromosomeType: ChromosomeType):ChromosomeCouple = {
    val c1:Chromosome = Chromosome(chromosomeType,gs1: _*)
    val c2:Chromosome = Chromosome(chromosomeType,gs2: _*)
    ChromosomeCouple(c1,c2)
  }

  private def buildStructuralChromosomeCouple:ChromosomeCouple = {
    val sgs1:Seq[MGene] = _createGeneCouples(StructuralGene,structuralChromosomeGenes,List())
    val sgs2:Seq[MGene] = _createGeneCouples(StructuralGene,structuralChromosomeGenes,List())
    buildChromosomeCouple(sgs1,sgs2,ChromosomeType.STRUCTURAL_ANIMAL)
  }

  private def buildLifeCycleChromosomeCouple:ChromosomeCouple = {
    val lcgs1:Seq[MGene] = _createGeneCouples(RegulatorGene,lifeCycleChromosomeGenes,List())
    val lcgs2:Seq[MGene] = _createGeneCouples(RegulatorGene,lifeCycleChromosomeGenes,List())
    buildChromosomeCouple(lcgs1,lcgs2,ChromosomeType.LIFE_CYCLE)
  }

  private def buildFeedingChromosomeCouple:ChromosomeCouple = {
    buildChromosomeCouple(feedingChromosomeGenes,feedingChromosomeGenes,ChromosomeType.FEEDING)
  }

  private def buildSexualChromosomeCouple:SexualChromosomeCouple = {
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

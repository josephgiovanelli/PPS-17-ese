package it.unibo.pps.ese.genetics

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
  feedingChromosomeGenes:Seq[Gene],
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
    val cc1 = Chromosome(ChromosomeType.COMMON,commonChromosomeGenes: _*)
    val cc2 = Chromosome(ChromosomeType.COMMON,commonChromosomeGenes: _*)
    val ccc = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    ccc.addChromosomeCouple(cc1,cc2)
    ccc
  }
  @tailrec
  private final def _createGeneCouples(
                          gt:GeneType,
                          gs:Seq[GeneWithPossibleAlleles],
                          gl:Seq[(GeneWithAllelicForms)])
  :Seq[(Gene)] = gs match {
    case h::t =>
      val map:Map[Seq[ProteinoGenicAmminoacid],Double] = h.alleles.map(a=>(a.allelicSeq,a.probability)).toMap
      val g1:GeneWithAllelicForms = GeneWithAllelicForms(h.baseGeneSeq,Utilities.sample(map),gt)
      _createGeneCouples(gt,t,gl :+ g1)
    case _ =>gl
  }

  private def buildStructuralChromosomeCouple:ChromosomeCouple = {

    val sgs1:Seq[Gene] = _createGeneCouples(StructuralGene,structuralChromosomeGenes,List())
    val sgs2:Seq[Gene] = _createGeneCouples(StructuralGene,structuralChromosomeGenes,List())
    val sc1:Chromosome = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL,sgs1: _*)
    val sc2:Chromosome = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL,sgs2: _*)
    val scc = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    scc.addChromosomeCouple(sc1,sc2)
    scc
  }
  private def buildLifeCycleChromosomeCouple:ChromosomeCouple = {
    val lcgs1:Seq[Gene] = _createGeneCouples(RegulatorGene,lifeCycleChromosomeGenes,List())
    val lcgs2:Seq[Gene] = _createGeneCouples(RegulatorGene,lifeCycleChromosomeGenes,List())
    val lcc1:Chromosome = Chromosome(ChromosomeType.LIFE_CYCLE,lcgs1: _*)
    val lcc2:Chromosome = Chromosome(ChromosomeType.LIFE_CYCLE,lcgs2: _*)
    val lccc = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    lccc.addChromosomeCouple(lcc1,lcc2)
    lccc
  }
  private def buildFeedingChromosomeCouple:ChromosomeCouple = {
    val fc1 = Chromosome(ChromosomeType.FEEDING,feedingChromosomeGenes: _*)
    val fc2 = Chromosome(ChromosomeType.FEEDING,feedingChromosomeGenes: _*)
    val fcc = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    fcc.addChromosomeCouple(fc1,fc2)
    fcc
  }

  private def buildSexualChromosomeCouple:SexualChromosomeCouple = {
    val sgs1:Seq[Gene] = _createGeneCouples(RegulatorGene,sexualChromosomeGenes,List())
    val sgs2:Seq[Gene] = _createGeneCouples(RegulatorGene,sexualChromosomeGenes,List())
    def _secondChromosome:SexualChromosome = Utilities.pickRandomElement(Male, Female) match {
      case Male =>
        Chromosome(ChromosomeType.SEXUAL_Y, Y)
      case Female =>
        Chromosome(ChromosomeType.SEXUAL_X, X, sgs2: _*)
    }
    val cs1:SexualChromosome = Chromosome(ChromosomeType.SEXUAL_X, X, sgs1: _*)
    val cs2:SexualChromosome = _secondChromosome
    val sxcc = new ChromosomeCoupleImpl with SexualChromosomeCouple {
      type ChromosomeUnit = SexualChromosome
    }
    sxcc.addChromosomeCouple(cs1,cs2)
    sxcc
  }


}

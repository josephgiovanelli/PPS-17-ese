package it.unibo.pps.ese.genetics.dna

object ChromosomeType extends Enumeration {
  type ChromosomeType = Value
  protected case class Val(trueSequence: Seq[GeneType]) extends super.Val{
  }
  implicit def valueToChromosomeType(x: Value): Val = x.asInstanceOf[Val]

  val COMMON = Val(List(IdentifierGene,IdentifierGene))
  val STRUCTURAL_PLANT = Val(List(StructuralGene,StructuralGene,StructuralGene))
  val ONLY_FOR_TEST = Val(List(RegulatorGene))
  val STRUCTURAL_ANIMAL = Val(List())
  val LIFE_CYCLE = Val(List(RegulatorGene,RegulatorGene,RegulatorGene,RegulatorGene,RegulatorGene))
  val FEEDING = Val(List(IdentifierGene))
  val SEXUAL_X = Val(List(RegulatorGene,RegulatorGene))
  val SEXUAL_Y = Val(List())
  def checkListOfGene(chromosomeType: ChromosomeType,seq: Seq[GeneType]):Boolean= chromosomeType match {
    case STRUCTURAL_ANIMAL => seq.forall(_ == StructuralGene)
    case _ =>chromosomeType.trueSequence.equals(seq)
  }
}

sealed trait SexualChromosomeType

case object X extends SexualChromosomeType
case object Y extends SexualChromosomeType


import it.unibo.pps.ese.genetics.dna.ChromosomeType.{ChromosomeType, _}

sealed trait Chromosome{
  def chromosomeType:ChromosomeType
  def geneList:Seq[MGene]
}

object Chromosome{
  def apply(chromosomeType: ChromosomeType,geneList:MGene*): Chromosome = new BasicChromosomeImpl(chromosomeType,geneList)

  def apply(chromosomeType: ChromosomeType,
            sexualChromosome: SexualChromosomeType,
            geneList:MGene*): SexualChromosome = new SexualChromosomeImpl(chromosomeType,
    sexualChromosome,
    geneList)
}


sealed trait SexualChromosome extends Chromosome{
  def sexualChromosome: SexualChromosomeType
}




abstract class BasicChromosome(
                                override val chromosomeType: ChromosomeType,
                                override val geneList:Seq[MGene]) extends Chromosome{
//  println(chromosomeType+geneList.map(_.geneType).toString())
  require(checkListOfGene(chromosomeType,geneList.map(_.geneType)))
}

class BasicChromosomeImpl(chromosomeType: ChromosomeType,geneList:Seq[MGene])
  extends BasicChromosome(chromosomeType,geneList){

  override def toString: String = chromosomeType.toString+"->"+geneList

}

class SexualChromosomeImpl(chromosomeType: ChromosomeType,
                           override val sexualChromosome: SexualChromosomeType,
                           geneList:Seq[MGene])
  extends BasicChromosome(chromosomeType,geneList) with SexualChromosome{

}


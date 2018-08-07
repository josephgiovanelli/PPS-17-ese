package it.unibo.pps.ese.genetics

object ChromosomeType extends Enumeration {
  type ChromosomeType = Value
  protected case class Val(trueSequence: Seq[GeneType]) extends super.Val{
  }
  implicit def valueToChromosomeType(x: Value): Val = x.asInstanceOf[Val]

  val COMMON = Val(List(IdentifierGene,IdentifierGene))
  val STRUCTURAL_PLANT = Val(List(StructuralGene,StructuralGene,StructuralGene))
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

sealed trait Gender
case object Male extends Gender
case object Female extends Gender

import ChromosomeType.{ChromosomeType, _}

case class CommonChromosome(reignGene:BasicGene,speciesGene:BasicGene)
trait ChromosomeCouple{
  type ChromosomeUnit <: {
    def geneList: Seq[Gene]
    def chromosomeType:ChromosomeType
  }
  def addChromosomeCouple(c1:ChromosomeUnit, c2:ChromosomeUnit)
  def addFirstChromosome(c:ChromosomeUnit)
  def addSecondChromosome(c:ChromosomeUnit)
  def firstChromosome:ChromosomeUnit
  def secondChromosome:ChromosomeUnit
}

trait SexualChromosomeCouple extends ChromosomeCouple{
  type ChromosomeUnit <: {
    def sexualChromosome:SexualChromosomeType
    def geneList: Seq[Gene]
    def chromosomeType:ChromosomeType
  }
  def gender:Gender = (firstChromosome.sexualChromosome,secondChromosome.sexualChromosome) match {
    case (X,X) => Female
    case (X,Y) => Male
    case (Y,X) => Male
    case _=> throw new IllegalStateException()
  }
}

sealed trait Chromosome{
  def chromosomeType:ChromosomeType
  def geneList:Seq[Gene]
}

sealed trait SexualChromosome extends Chromosome{
  def sexualChromosome: SexualChromosomeType
}

sealed trait Genome{
  def autosomeChromosomeCouples:Map[ChromosomeType,ChromosomeCouple]
  def firstGenomeSequence:Map[ChromosomeType,Chromosome]
  def secondGenomeSequence:Map[ChromosomeType,Chromosome]
}
sealed trait AnimalGenome extends Genome{
  def sexualChromosomeCouple:SexualChromosomeCouple
  def firstSexualChromosome:SexualChromosome
  def secondSexualChromosome:SexualChromosome
}
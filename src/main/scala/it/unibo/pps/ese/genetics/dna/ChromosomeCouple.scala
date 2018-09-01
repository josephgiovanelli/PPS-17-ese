package it.unibo.pps.ese.genetics.dna

import it.unibo.pps.ese.genetics.dna.ChromosomeType.ChromosomeType
import it.unibo.pps.ese.genetics.entities.{Female, Gender, Male}

trait ChromosomeCouple{
  type ChromosomeUnit <: Chromosome
  def addChromosomeCouple(c1:ChromosomeUnit, c2:ChromosomeUnit)
  def addFirstChromosome(c:ChromosomeUnit)
  def addSecondChromosome(c:ChromosomeUnit)
  def firstChromosome:ChromosomeUnit
  def secondChromosome:ChromosomeUnit
}
object ChromosomeCouple{
  def apply(c1: Chromosome,c2: Chromosome): ChromosomeCouple = {
    val cc = new ChromosomeCoupleImpl {
      type ChromosomeUnit = Chromosome
    }
    cc.addChromosomeCouple(c1,c2)
    cc
  }
}

trait SexualChromosomeCouple extends ChromosomeCouple{
  type ChromosomeUnit <: SexualChromosome
  def gender:Gender = (firstChromosome.sexualChromosome,secondChromosome.sexualChromosome) match {
    case (X,X) => Female
    case (X,Y) => Male
    case (Y,X) => Male
    case _=> throw new IllegalStateException()
  }
}
object SexualChromosomeCouple{
  def apply(sc1:SexualChromosome,sc2:SexualChromosome): SexualChromosomeCouple ={
    val scc = new ChromosomeCoupleImpl with SexualChromosomeCouple {
      type ChromosomeUnit = SexualChromosome
    }
    scc.addChromosomeCouple(sc1,sc2)
    scc
  }
}
abstract class ChromosomeCoupleImpl() extends ChromosomeCouple{
  private var couple:Map[Int,ChromosomeUnit] = Map()
  override def addChromosomeCouple(c1: ChromosomeUnit, c2: ChromosomeUnit): Unit = {
    _assignCouple(c1,c2)
  }
  override def addFirstChromosome(c: ChromosomeUnit): Unit = _assignCouple(c,couple(2))

  override def addSecondChromosome(c: ChromosomeUnit): Unit = _assignCouple(couple(1),c)

  override def firstChromosome: ChromosomeUnit = couple(1)

  override def secondChromosome: ChromosomeUnit = couple(2)

  private def _checkConsistency(c1:ChromosomeUnit, c2:ChromosomeUnit):Boolean = {
    c1.chromosomeType==c2.chromosomeType ||
      c1.chromosomeType == ChromosomeType.SEXUAL_X||
      c1.chromosomeType == ChromosomeType.SEXUAL_Y
  }

  private def _assignCouple(c1:ChromosomeUnit, c2:ChromosomeUnit):Unit = _checkConsistency(c1,c2) match {
    case true=> couple = Map(1->c1,2->c2)
    case _=> throw new IllegalArgumentException()
  }
}
